use serde_json::{Map, Number, Value as J};
use std::collections::HashMap;
use std::path::Path;

pub const ENC: u64 = 2;

pub fn load_golden(path: &Path) -> J {
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let v: J = serde_json::from_str(&text).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    expand_golden(&v).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

pub fn compact_golden(g: &J) -> J {
    match try_compact(g) {
        Some(c) if expand_golden(&c).as_ref() == Ok(g) => c,
        _ => g.clone(),
    }
}

pub fn is_compact(g: &J) -> bool {
    g.get("enc").is_some()
}

fn try_compact(g: &J) -> Option<J> {
    let mut out = g.as_object()?.clone();
    if out.contains_key("enc") {
        return None;
    }
    if let Some(J::Array(items)) = out.get_mut("items") {
        let mut prev: HashMap<String, Vec<J>> = HashMap::new();
        for it in items.iter_mut() {
            compact_item(it, &mut prev);
        }
    }
    out.insert("enc".into(), J::from(ENC));
    Some(J::Object(out))
}

fn read_range(it: &J) -> Option<String> {
    if it.get("op")?.as_str()? != "read" {
        return None;
    }
    it.get("cells")?.as_array()?;
    Some(it.get("range")?.as_str()?.to_string())
}

fn cell_names(cells: &[J]) -> Option<Vec<&str>> {
    cells.iter().map(|c| c.get("cell")?.as_str()).collect()
}

fn compact_item(it: &mut J, prev: &mut HashMap<String, Vec<J>>) {
    if let Some(r) = read_range(it) {
        let cur: Vec<J> = it["cells"].as_array().unwrap().clone();
        if let (Some(p), Some(cn)) = (prev.get(&r), cell_names(&cur)) {
            let uniq = {
                let mut s: Vec<&str> = cn.clone();
                s.sort_unstable();
                s.windows(2).all(|w| w[0] != w[1])
            };
            if uniq && cell_names(p).as_deref() == Some(&cn[..]) {
                let chg: Vec<J> = cur
                    .iter()
                    .zip(p)
                    .filter(|(a, b)| a != b)
                    .map(|(a, _)| a.clone())
                    .collect();
                let m = it.as_object_mut().unwrap();
                m.remove("cells");
                m.insert("chg".into(), J::Array(chg));
            }
        }
        prev.insert(r, cur);
    }
    slim_record(it);
    if let Some(m) = it.as_object_mut() {
        for k in ["cells", "chg"] {
            if let Some(J::Array(a)) = m.get_mut(k) {
                a.iter_mut().for_each(slim_record);
            }
        }
    }
    slim_nums(it);
}

fn slim_record(r: &mut J) {
    let Some(m) = r.as_object_mut() else { return };
    if m.contains_key("value") {
        match m.get("format") {
            Some(J::String(s)) if s == "General" => {
                m.remove("format");
            }
            None => {
                m.insert("format".into(), J::Null);
            }
            _ => {}
        }
    }
    if m.contains_key("host") && m.contains_key("formula") {
        match m.get("normalized") {
            Some(n) if n == &m["formula"] => {
                m.remove("normalized");
            }
            None => {
                m.insert("normalized".into(), J::Null);
            }
            _ => {}
        }
    }
}

fn unslim_record(r: &mut J) {
    let Some(m) = r.as_object_mut() else { return };
    if m.contains_key("value") {
        match m.get("format") {
            None => {
                m.insert("format".into(), J::String("General".into()));
            }
            Some(J::Null) => {
                m.remove("format");
            }
            _ => {}
        }
    }
    if m.contains_key("host") && m.contains_key("formula") {
        match m.get("normalized") {
            None => {
                let f = m["formula"].clone();
                m.insert("normalized".into(), f);
            }
            Some(J::Null) => {
                m.remove("normalized");
            }
            _ => {}
        }
    }
}

fn num_bits(m: &Map<String, J>) -> Option<u64> {
    if m.get("t")?.as_str()? != "n" {
        return None;
    }
    u64::from_str_radix(m.get("bits")?.as_str()?, 16).ok()
}

fn slim_nums(v: &mut J) {
    match v {
        J::Object(m) => {
            if let (Some(bits), Some(J::Number(n))) = (num_bits(m), m.get("v")) {
                if n.is_f64() && n.as_f64().map(f64::to_bits) == Some(bits) {
                    m.remove("v");
                }
            }
            m.values_mut().for_each(slim_nums);
        }
        J::Array(a) => a.iter_mut().for_each(slim_nums),
        _ => {}
    }
}

fn unslim_nums(v: &mut J) {
    match v {
        J::Object(m) => {
            if !m.contains_key("v") {
                if let Some(bits) = num_bits(m) {
                    if let Some(n) = Number::from_f64(f64::from_bits(bits)) {
                        m.insert("v".into(), J::Number(n));
                    }
                }
            }
            m.values_mut().for_each(unslim_nums);
        }
        J::Array(a) => a.iter_mut().for_each(unslim_nums),
        _ => {}
    }
}

pub fn expand_golden(g: &J) -> Result<J, String> {
    let mut out = g.as_object().ok_or("golden is not an object")?.clone();
    match out.remove("enc") {
        None => return Ok(g.clone()),
        Some(e) if e.as_u64() == Some(ENC) => {}
        Some(e) => return Err(format!("unknown golden encoding {e}")),
    }
    if let Some(J::Array(items)) = out.get_mut("items") {
        let mut prev: HashMap<String, Vec<J>> = HashMap::new();
        for it in items.iter_mut() {
            expand_item(it, &mut prev)?;
        }
    }
    Ok(J::Object(out))
}

fn expand_item(it: &mut J, prev: &mut HashMap<String, Vec<J>>) -> Result<(), String> {
    unslim_nums(it);
    unslim_record(it);
    let Some(m) = it.as_object_mut() else {
        return Ok(());
    };
    for k in ["cells", "chg"] {
        if let Some(J::Array(a)) = m.get_mut(k) {
            a.iter_mut().for_each(unslim_record);
        }
    }
    let is_read = m.get("op").and_then(J::as_str) == Some("read");
    let range = m.get("range").and_then(J::as_str).map(str::to_string);
    if let Some(J::Array(chg)) = m.remove("chg") {
        let r = range.as_ref().ok_or("chg without range")?;
        let p = prev.get(r).ok_or("chg without previous read")?;
        let mut j = 0;
        let mut cells = Vec::with_capacity(p.len());
        for c in p {
            if j < chg.len() && chg[j].get("cell") == c.get("cell") {
                cells.push(chg[j].clone());
                j += 1;
            } else {
                cells.push(c.clone());
            }
        }
        if j != chg.len() {
            return Err("chg cell not in previous read".into());
        }
        m.insert("cells".into(), J::Array(cells));
    }
    if is_read {
        if let (Some(r), Some(J::Array(c))) = (range, m.get("cells")) {
            prev.insert(r, c.clone());
        }
    }
    Ok(())
}
