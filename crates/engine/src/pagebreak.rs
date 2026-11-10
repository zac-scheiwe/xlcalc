use crate::address::SheetId;
use crate::workbook::Workbook;

impl Workbook {
    pub fn page_breaks(&self, sheet: SheetId, rows: bool) -> Vec<u32> {
        let used = self.used_range(sheet);
        let limit = used.map_or(0, |u| if rows { u.r1 } else { u.c1 }) + 1;
        self.views.get(&sheet).map_or_else(Vec::new, |v| {
            let s = if rows { &v.hbreaks } else { &v.vbreaks };
            s.iter().copied().filter(|&b| b <= limit).collect()
        })
    }

    pub fn add_page_break(&mut self, sheet: SheetId, rows: bool, at: u32) {
        let v = self.view_mut(sheet);
        if rows {
            v.hbreaks.insert(at);
        } else {
            v.vbreaks.insert(at);
        }
    }

    pub fn delete_page_break(&mut self, sheet: SheetId, rows: bool, at: u32) {
        if let Some(v) = self.views.get_mut(&sheet) {
            if rows {
                v.hbreaks.remove(&at);
            } else {
                v.vbreaks.remove(&at);
            }
        }
    }

    pub fn reset_page_breaks(&mut self, sheet: SheetId) {
        if let Some(v) = self.views.get_mut(&sheet) {
            v.hbreaks.clear();
            v.vbreaks.clear();
        }
    }
}
