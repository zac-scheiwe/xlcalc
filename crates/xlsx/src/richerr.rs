use xlcalc::XlError;

#[derive(Default)]
pub(crate) struct RichErrors {
    pub(crate) dynamic: bool,

    values: Vec<(usize, Vec<String>)>,

    structs: Vec<usize>,
}

const STRUCTS: [&[(&str, char)]; 3] = [
    &[("errorType", 'i'), ("subType", 'i')],
    &[("errorType", 'i'), ("field", 's'), ("subType", 'i')],
    &[
        ("colOffset", 'i'),
        ("errorType", 'i'),
        ("rwOffset", 'i'),
        ("subType", 'i'),
    ],
];

impl RichErrors {
    pub(crate) fn vm(&mut self, e: XlError) -> Option<usize> {
        let (st, vals): (usize, Vec<&str>) = if e == XlError::SPILL {
            (2, vec!["0", "8", "1", "1"])
        } else if e == XlError::CALC {
            (0, vec!["13", "3"])
        } else if e == XlError::BLOCKED {
            (0, vec!["10", "9"])
        } else if e == XlError::FIELD {
            (1, vec!["12", "", "0"])
        } else if e == XlError::CONNECT {
            (0, vec!["9", "0"])
        } else if e == XlError::UNKNOWN {
            (0, vec!["11", "0"])
        } else {
            return None;
        };
        let vals: Vec<String> = vals.into_iter().map(str::to_string).collect();
        let pos = match self.values.iter().position(|v| v.0 == st && v.1 == vals) {
            Some(p) => p,
            None => {
                self.values.push((st, vals));
                self.values.len() - 1
            }
        };
        if !self.structs.contains(&st) {
            self.structs.push(st);
        }
        Some(pos + 1)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub(crate) fn values_xml(&self) -> String {
        let mut s = format!(
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n<rvData xmlns=\"http://schemas.microsoft.com/office/spreadsheetml/2017/richdata\" count=\"{}\">",
            self.values.len()
        );
        for (st, vals) in &self.values {
            let si = self.structs.iter().position(|x| x == st).unwrap_or(0);
            s.push_str(&format!("<rv s=\"{si}\">"));
            for v in vals {
                s.push_str(&format!("<v>{}</v>", crate::rawxml::escape_text(v)));
            }
            s.push_str("</rv>");
        }
        s.push_str("</rvData>");
        s
    }

    pub(crate) fn structure_xml(&self) -> String {
        let mut s = format!(
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n<rvStructures xmlns=\"http://schemas.microsoft.com/office/spreadsheetml/2017/richdata\" count=\"{}\">",
            self.structs.len()
        );
        for st in &self.structs {
            s.push_str("<s t=\"_error\">");
            for (k, t) in STRUCTS[*st] {
                s.push_str(&format!("<k n=\"{k}\" t=\"{t}\"/>"));
            }
            s.push_str("</s>");
        }
        s.push_str("</rvStructures>");
        s
    }

    pub(crate) fn types_xml() -> &'static str {
        include_str!("richerr_types.xml")
    }

    pub(crate) fn metadata_xml(&self) -> String {
        let dynamic = self.dynamic;
        let flags = "copy=\"1\" pasteAll=\"1\" pasteValues=\"1\" merge=\"1\" splitFirst=\"1\" rowColShift=\"1\" clearFormats=\"1\" clearComments=\"1\" assign=\"1\" coerce=\"1\"";
        let rich = !self.is_empty();
        let mut s = String::from(
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n<metadata xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"",
        );
        if rich {
            s.push_str(
                " xmlns:xlrd=\"http://schemas.microsoft.com/office/spreadsheetml/2017/richdata\"",
            );
        }
        s.push_str(
            " xmlns:xda=\"http://schemas.microsoft.com/office/spreadsheetml/2017/dynamicarray\">",
        );
        s.push_str(&format!(
            "<metadataTypes count=\"{}\">",
            usize::from(dynamic) + usize::from(rich)
        ));
        if dynamic {
            s.push_str(&format!(
                "<metadataType name=\"XLDAPR\" minSupportedVersion=\"120000\" {flags} cellMeta=\"1\"/>"
            ));
        }
        if rich {
            s.push_str(&format!(
                "<metadataType name=\"XLRICHVALUE\" minSupportedVersion=\"120000\" {flags}/>"
            ));
        }
        s.push_str("</metadataTypes>");
        if dynamic {
            s.push_str("<futureMetadata name=\"XLDAPR\" count=\"1\"><bk><extLst><ext uri=\"{bdbb8cdc-fa1e-496e-a857-3c3f30c029c3}\"><xda:dynamicArrayProperties fDynamic=\"1\" fCollapsed=\"0\"/></ext></extLst></bk></futureMetadata>");
        }
        if rich {
            s.push_str(&format!(
                "<futureMetadata name=\"XLRICHVALUE\" count=\"{}\">",
                self.values.len()
            ));
            for i in 0..self.values.len() {
                s.push_str(&format!("<bk><extLst><ext uri=\"{{3e2802c4-a4d2-4d8b-9148-e3be6c30e623}}\"><xlrd:rvb i=\"{i}\"/></ext></extLst></bk>"));
            }
            s.push_str("</futureMetadata>");
        }
        if dynamic {
            s.push_str("<cellMetadata count=\"1\"><bk><rc t=\"1\" v=\"0\"/></bk></cellMetadata>");
        }
        if rich {
            s.push_str(&format!("<valueMetadata count=\"{}\">", self.values.len()));
            let t = if dynamic { 2 } else { 1 };
            for i in 0..self.values.len() {
                s.push_str(&format!("<bk><rc t=\"{t}\" v=\"{i}\"/></bk>"));
            }
            s.push_str("</valueMetadata>");
        }
        s.push_str("</metadata>");
        s
    }
}
