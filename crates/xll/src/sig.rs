#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Short01,

    Double,

    DoublePtr,

    Ushort,

    Short,

    Int,

    BoolPtr,

    Ansi,

    AnsiCounted,

    Wide,

    WideCounted,

    Fp12,

    Fp,

    Oper,

    OperRef,

    OperOld,

    OperRefOld,

    AsyncHandle,

    Void,
}

impl Ty {
    fn parse(c: char, wide: bool) -> Option<Ty> {
        Some(match (c, wide) {
            ('A', _) => Ty::Short01,
            ('B', _) => Ty::Double,
            ('E', _) => Ty::DoublePtr,
            ('H', _) => Ty::Ushort,
            ('I', _) => Ty::Short,
            ('J', _) => Ty::Int,
            ('L', _) => Ty::BoolPtr,
            ('C' | 'F', false) => Ty::Ansi,
            ('D' | 'G', false) => Ty::AnsiCounted,
            ('C' | 'F', true) => Ty::Wide,
            ('D' | 'G', true) => Ty::WideCounted,
            ('K', true) => Ty::Fp12,
            ('K', false) => Ty::Fp,
            ('P', _) => Ty::OperOld,
            ('Q', _) => Ty::Oper,
            ('R', _) => Ty::OperRefOld,
            ('U', _) => Ty::OperRef,
            ('X', _) => Ty::AsyncHandle,
            ('>', _) => Ty::Void,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sig {
    pub ret: Ty,
    pub args: Vec<Ty>,

    pub volatile: bool,

    pub thread_safe: bool,

    pub macro_sheet: bool,

    pub cluster_safe: bool,

    pub is_async: bool,
}

pub fn parse(text: &str) -> Option<Sig> {
    let mut chars: Vec<char> = text.chars().collect();
    let (mut volatile, mut thread_safe, mut macro_sheet, mut cluster_safe) =
        (false, false, false, false);

    while let Some(&c) = chars.last() {
        match c {
            '!' => volatile = true,
            '$' => thread_safe = true,
            '#' => macro_sheet = true,
            '&' => cluster_safe = true,
            _ => break,
        }
        chars.pop();
    }
    let mut types = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        let wide = chars.get(i + 1) == Some(&'%');
        types.push(Ty::parse(chars[i], wide)?);
        i += if wide { 2 } else { 1 };
    }
    let (&ret, args) = types.split_first()?;
    let is_async = args.last() == Some(&Ty::AsyncHandle);
    if args.iter().rev().skip(1).any(|t| *t == Ty::AsyncHandle) || (ret == Ty::Void && !is_async) {
        return None;
    }
    Some(Sig {
        ret,
        args: args.to_vec(),
        volatile,
        thread_safe,
        macro_sheet,
        cluster_safe,
        is_async,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_and_modified_texts() {
        let s = parse("QQ$").unwrap();
        assert_eq!(s.ret, Ty::Oper);
        assert_eq!(s.args, vec![Ty::Oper]);
        assert!(s.thread_safe && !s.volatile);
        let s = parse("BBJC%!#").unwrap();
        assert_eq!(s.args, vec![Ty::Double, Ty::Int, Ty::Wide]);
        assert!(s.volatile && s.macro_sheet);
        let s = parse(">BX").unwrap();
        assert!(s.is_async);
        assert_eq!(s.ret, Ty::Void);
        assert!(parse("QZ").is_none());
        assert!(parse(">B").is_none());
    }
}
