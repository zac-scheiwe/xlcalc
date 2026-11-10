use super::cur::Cur;
use super::read::{ref8, Fx};
use super::Rec;
use crate::xlsb::{esc, Dec};
use std::fmt::Write as _;

#[derive(Default, Clone)]
pub(super) struct Field {
    pub id: u32,

    pub ilta: u32,
    pub flags: u32,
    pub name: String,
    pub label: Option<String>,

    pub formula: Option<Vec<u8>>,
}

#[derive(Default, Clone)]
pub(super) struct Feat {
    pub rect: (u32, u32, u32, u32),
    pub id: u32,
    pub header: u32,
    pub totals: u32,
    pub flags: u32,
    pub name: String,
    pub fields: Vec<Field>,
    pub display: Option<String>,

    pub style: Option<(u16, String)>,
}

pub(super) const FN_NAMES: [&str; 10] = [
    "",
    "average",
    "count",
    "countNums",
    "max",
    "min",
    "sum",
    "stdDev",
    "var",
    "custom",
];

pub(super) fn feature(r: &Rec<'_>, cp: u16) -> Result<Feat, String> {
    let mut full = r.d.to_vec();
    for c in &r.cont {
        full.extend_from_slice(c);
    }
    let mut c = Cur::new(&full);
    c.take(12)?;
    let isf = c.u16()?;
    if isf != 5 {
        return Err(format!("feature type {isf} is not a list"));
    }
    c.take(5)?;
    let cref = c.u16()?;
    c.take(6)?;
    let mut rect = (0, 0, 0, 0);
    for i in 0..cref {
        let x = ref8(&mut c)?;
        if i == 0 {
            rect = x;
        }
    }
    let mut f = Feat {
        rect,
        ..Feat::default()
    };
    c.u32()?;
    f.id = c.u32()?;
    f.header = c.u32()?;
    f.totals = c.u32()?;
    c.u32()?;
    c.u32()?;
    c.take(4)?;
    f.flags = c.u32()?;
    c.take(32)?;
    f.name = c.long_str(cp)?;
    let n = c.u16()?;
    c.long_str(cp)?;
    for _ in 0..n {
        let mut fl = Field {
            id: c.u32()?,
            ..Field::default()
        };
        c.take(8)?;
        fl.ilta = c.u32()?;
        c.take(8)?;
        fl.flags = c.u32()?;
        c.take(8)?;
        c.long_str(cp)?;
        fl.name = c.long_str(cp)?;
        let tail = |c: &mut Cur<'_>, fl: &mut Field| -> Result<(), String> {
            if fl.flags & 0x400 != 0 {
                fl.label = Some(c.long_str(cp)?);
            }
            if fl.flags & 0x80 != 0 {
                let cce = usize::from(c.u16()?);
                fl.formula = Some(c.take(cce)?.to_vec());
            }
            Ok(())
        };
        if f.header == 0 {
            tail(&mut c, &mut fl)?;
            c.take(4)?;
        } else {
            if fl.flags & 0x800 != 0 {
                c.take(14)?;
            } else if fl.flags & 1 != 0 {
                c.take(6)?;
            }
            tail(&mut c, &mut fl)?;
        }
        f.fields.push(fl);
    }
    Ok(f)
}

pub(super) fn list12(r: &Rec<'_>, feats: &mut [Feat], cp: u16) -> Result<(), String> {
    let mut c = Cur::new(r.d);
    c.take(12)?;
    let lt = c.u16()?;
    let id = c.u32()?;
    let Some(f) = feats.iter_mut().find(|f| f.id == id) else {
        return Ok(());
    };
    match lt {
        1 => {
            let flags = c.u16()?;
            f.style = Some((flags, c.long_str(cp)?));
        }
        2 => f.display = Some(c.long_str(cp)?),
        _ => {}
    }
    Ok(())
}

fn rect_str(r: (u32, u32, u32, u32)) -> String {
    crate::xlsb::rect_name(r.0, r.1, r.2, r.3)
}

pub(super) fn to_xml(
    f: &Feat,
    fx: &mut Fx,
    cp: u16,
    idx: u32,
    sheet: &str,
    warnings: &mut Vec<String>,
    calc: &dyn Fn(u32, u32) -> Option<String>,
) -> String {
    let name = if f.name.is_empty() {
        format!("Table{}", f.id)
    } else {
        f.name.clone()
    };
    let display = f.display.clone().unwrap_or_else(|| name.clone());
    let (r0, c0, r1, c1) = f.rect;
    let mut x = String::new();
    let _ = write!(
        x,
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<table xmlns=\"{}\" id=\"{}\" name=\"{}\" displayName=\"{}\" ref=\"{}\"",
        super::sheet::MAIN,
        f.id,
        esc(&name),
        esc(&display),
        rect_str(f.rect)
    );
    if f.header == 0 {
        x.push_str(" headerRowCount=\"0\"");
    }
    if f.totals > 0 {
        let _ = write!(x, " totalsRowCount=\"{}\"", f.totals);
    } else {
        x.push_str(" totalsRowShown=\"0\"");
    }
    x.push('>');
    if f.header > 0 && f.flags & 0x02 != 0 {
        let end = r1.saturating_sub(f.totals);
        let _ = write!(x, "<autoFilter ref=\"{}\"/>", rect_str((r0, c0, end, c1)));
    }
    let _ = write!(x, "<tableColumns count=\"{}\">", f.fields.len());
    for (i, fl) in f.fields.iter().enumerate() {
        let _ = write!(
            x,
            "<tableColumn id=\"{}\" name=\"{}\"",
            fl.id,
            esc(&fl.name)
        );
        let func = FN_NAMES.get(fl.ilta as usize).copied().unwrap_or("");
        if !func.is_empty() {
            let _ = write!(x, " totalsRowFunction=\"{func}\"");
        }
        if let Some(l) = &fl.label {
            let _ = write!(x, " totalsRowLabel=\"{}\"", esc(l));
        }
        let formula =
            fl.formula
                .as_ref()
                .and_then(|g| match fx.text(g, &[], (0, 0), Some(idx), cp) {
                    Ok(Dec::Text(t)) => Some(t),
                    Ok(_) => None,
                    Err(e) => {
                        warnings.push(format!("{sheet}: table {name}: totals formula: {e}"));
                        None
                    }
                });
        let cf = (fl.flags & 0x800 != 0)
            .then(|| calc(r0 + u32::from(f.header > 0), c0 + i as u32))
            .flatten();
        match (formula, cf) {
            (Some(t), _) => {
                let _ = write!(
                    x,
                    "><totalsRowFormula>{}</totalsRowFormula></tableColumn>",
                    esc(&t)
                );
            }
            (None, Some(t)) => {
                let _ = write!(
                    x,
                    "><calculatedColumnFormula>{}</calculatedColumnFormula></tableColumn>",
                    esc(&t)
                );
            }
            (None, None) => x.push_str("/>"),
        }
    }
    x.push_str("</tableColumns>");
    if let Some((fl, sname)) = &f.style {
        x.push_str("<tableStyleInfo");
        if fl & 0x40 != 0 && !sname.is_empty() {
            let _ = write!(x, " name=\"{}\"", esc(sname));
        }
        for (bit, key) in [
            (1u16, "showFirstColumn"),
            (2, "showLastColumn"),
            (4, "showRowStripes"),
            (8, "showColumnStripes"),
        ] {
            let _ = write!(x, " {key}=\"{}\"", u8::from(fl & bit != 0));
        }
        x.push_str("/>");
    }
    x.push_str("</table>");
    x
}
