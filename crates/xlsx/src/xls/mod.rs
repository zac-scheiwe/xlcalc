mod cf12;
mod cur;
mod props;
mod ptg;
mod ptg_out;
mod rcf;
mod rchart;
mod rcomments;
mod rdraw;
mod read;
pub(crate) use read::protect_xml;
mod rextras;
mod rpivot;
mod rstyles;
mod rtable;
mod sheet;
mod tstyle;
mod wblob;
mod wcf;
mod wchart;
mod wcolor;
mod wcomments;
mod wdraw;
mod wext;
mod wextras;
mod wpcalc;
mod wpivot;
mod write;
mod wsheet;
mod wstyles;
mod wtable;

pub(crate) struct Rec<'a> {
    pub id: u16,
    pub d: &'a [u8],
    pub cont: Vec<&'a [u8]>,
}

pub(crate) use read::{is_cfb, to_xlsx};
pub use write::xlsx_to_xls;
