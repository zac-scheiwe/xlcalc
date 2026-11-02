use crate::rawxml::{self, Tag};
use std::collections::BTreeMap;
use std::io::Read;
use xlcalc::SheetId;

#[derive(Clone, Debug, PartialEq)]
pub struct Rel {
    pub id: String,

    pub ty: String,

    pub target: String,

    pub external: bool,
}

impl Rel {
    pub fn kind(&self) -> &str {
        self.ty.rsplit('/').next().unwrap_or("")
    }
}

#[derive(Clone, Debug)]
pub struct SourceSheet {
    pub id: SheetId,

    pub part: String,

    pub rel_id: String,

    pub sheet_id: u32,
}

#[derive(Clone, Debug, Default)]
pub struct Package {
    parts: Vec<(String, Vec<u8>)>,
    defaults: BTreeMap<String, String>,
    overrides: BTreeMap<String, String>,
    pub(crate) sheets: Vec<SourceSheet>,
    structural: Vec<SheetId>,

    pub(crate) active: Option<(SheetId, SheetId)>,
}

pub fn rels_name(p: &str) -> String {
    match p.rsplit_once('/') {
        Some((d, f)) => format!("{d}/_rels/{f}.rels"),
        None => format!("_rels/{p}.rels"),
    }
}

pub fn dir_of(p: &str) -> &str {
    p.rsplit_once('/').map_or("", |(d, _)| d)
}

pub fn resolve(base_dir: &str, target: &str) -> String {
    if let Some(t) = target.strip_prefix('/') {
        return t.to_string();
    }
    let mut parts: Vec<&str> = base_dir.split('/').filter(|s| !s.is_empty()).collect();
    for seg in target.split('/') {
        match seg {
            ".." => {
                parts.pop();
            }
            "." | "" => {}
            s => parts.push(s),
        }
    }
    parts.join("/")
}

pub fn relative(from: &str, to: &str) -> String {
    let f: Vec<&str> = from.split('/').filter(|s| !s.is_empty()).collect();
    let t: Vec<&str> = to.split('/').collect();
    let common = f.iter().zip(&t).take_while(|(a, b)| a == b).count();
    let mut out: Vec<&str> = vec![".."; f.len() - common];
    out.extend(&t[common..]);
    out.join("/")
}

pub fn parse_rels(bytes: &[u8]) -> Result<Vec<Rel>, String> {
    let s = rawxml::split(bytes)?;
    let mut out = Vec::new();
    for k in s.kids.iter().filter(|k| k.name == "Relationship") {
        let t = Tag::parse(&k.raw)?;
        out.push(Rel {
            id: t.value("Id").unwrap_or_default(),
            ty: t.value("Type").unwrap_or_default(),
            target: t.value("Target").unwrap_or_default(),
            external: t.value("TargetMode").as_deref() == Some("External"),
        });
    }
    Ok(out)
}

pub fn write_rels(rels: &[Rel]) -> Vec<u8> {
    let mut s = String::from(rawxml::DECL);
    s.push_str(
        "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
    );
    for r in rels {
        let mut t = Tag::new("Relationship");
        t.set("Id", Some(&r.id))
            .set("Type", Some(&r.ty))
            .set("Target", Some(&r.target));
        if r.external {
            t.set("TargetMode", Some("External"));
        }
        s.push_str(&t.empty());
    }
    s.push_str("</Relationships>");
    s.into_bytes()
}

impl Package {
    pub fn read(bytes: &[u8]) -> Result<Package, String> {
        let mut zip =
            zip::ZipArchive::new(std::io::Cursor::new(bytes)).map_err(|e| e.to_string())?;
        let mut pkg = Package::default();
        for i in 0..zip.len() {
            let mut f = zip.by_index(i).map_err(|e| e.to_string())?;
            if f.is_dir() {
                continue;
            }
            let name = f
                .name()
                .map_err(|e| e.to_string())?
                .trim_start_matches('/')
                .to_string();
            let mut v = Vec::new();
            f.read_to_end(&mut v).map_err(|e| format!("{name}: {e}"))?;
            pkg.parts.push((name, v));
        }
        if let Some(ct) = pkg.part("[Content_Types].xml") {
            let s = rawxml::split(ct)?;
            for k in &s.kids {
                let t = Tag::parse(&k.raw)?;
                let ty = t.value("ContentType").unwrap_or_default();
                match k.name.as_str() {
                    "Default" => {
                        let ext = t.value("Extension").unwrap_or_default().to_lowercase();
                        pkg.defaults.insert(ext, ty);
                    }
                    "Override" => {
                        let p = t.value("PartName").unwrap_or_default();
                        pkg.overrides
                            .insert(p.trim_start_matches('/').to_lowercase(), ty);
                    }
                    _ => {}
                }
            }
        }

        for i in 0..pkg.parts.len() {
            let ns = match pkg.content_type(&pkg.parts[i].0) {
                Some(t) if t.ends_with("extended-properties+xml") => rawxml::APP_NS,
                Some(t)
                    if t.contains("spreadsheetml.")
                        && [
                            "main+xml",
                            "worksheet+xml",
                            "sharedStrings+xml",
                            "styles+xml",
                            "table+xml",
                            "sheetMetadata+xml",
                            "calcChain+xml",
                        ]
                        .iter()
                        .any(|e| t.ends_with(e)) =>
                {
                    rawxml::MAIN_NS
                }
                _ => continue,
            };
            if let Some(b) = rawxml::unprefix(&pkg.parts[i].1, ns) {
                pkg.parts[i].1 = b;
            }
        }
        Ok(pkg)
    }

    pub fn part(&self, name: &str) -> Option<&[u8]> {
        let name = name.trim_start_matches('/');
        self.parts
            .iter()
            .find(|(n, _)| n.eq_ignore_ascii_case(name))
            .map(|(_, b)| b.as_slice())
    }

    pub fn part_names(&self) -> impl Iterator<Item = &str> {
        self.parts.iter().map(|(n, _)| n.as_str())
    }

    pub fn content_type(&self, name: &str) -> Option<&str> {
        let name = name.trim_start_matches('/').to_lowercase();
        if let Some(t) = self.overrides.get(&name) {
            return Some(t);
        }
        let ext = name.rsplit_once('.').map(|(_, e)| e)?;
        self.defaults.get(ext).map(String::as_str)
    }

    pub fn defaults(&self) -> &BTreeMap<String, String> {
        &self.defaults
    }

    pub fn rels(&self, owner: &str) -> Vec<Rel> {
        self.part(&rels_name(owner))
            .and_then(|b| parse_rels(b).ok())
            .unwrap_or_default()
    }

    pub fn source_sheet(&self, id: SheetId) -> Option<&SourceSheet> {
        self.sheets.iter().find(|s| s.id == id)
    }

    pub fn mark_structural_edit(&mut self, id: SheetId) {
        if !self.structural.contains(&id) {
            self.structural.push(id);
        }
    }

    pub(crate) fn active_sheet(&self, now: SheetId) -> SheetId {
        match self.active {
            Some((at_import, tab)) if at_import == now => tab,
            _ => now,
        }
    }

    pub(crate) fn structurally_edited(&self, id: SheetId) -> bool {
        self.structural.contains(&id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paths() {
        assert_eq!(
            resolve("xl/worksheets", "../tables/table1.xml"),
            "xl/tables/table1.xml"
        );
        assert_eq!(resolve("xl", "/xl/styles.xml"), "xl/styles.xml");
        assert_eq!(
            relative("xl/worksheets", "xl/tables/table1.xml"),
            "../tables/table1.xml"
        );
        assert_eq!(
            relative("xl", "xl/worksheets/sheet1.xml"),
            "worksheets/sheet1.xml"
        );
        assert_eq!(relative("", "xl/workbook.xml"), "xl/workbook.xml");
        assert_eq!(rels_name("xl/workbook.xml"), "xl/_rels/workbook.xml.rels");
    }
}
