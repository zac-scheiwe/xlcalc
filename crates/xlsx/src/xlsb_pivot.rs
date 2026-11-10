mod formula;
mod read;
mod write;

pub(crate) use read::{cache_def, table};
pub(crate) use write::{cache_def_bin, cache_records_bin, table_bin};

pub(super) const R_DEF: u32 = 179;
pub(super) const R_DEF_END: u32 = 180;
pub(super) const R_FIELDS: u32 = 181;
pub(super) const R_FIELDS_END: u32 = 182;
pub(super) const R_FIELD: u32 = 183;
pub(super) const R_FIELD_END: u32 = 184;
pub(super) const R_SRC: u32 = 185;
pub(super) const R_SRC_END: u32 = 186;
pub(super) const R_SHEETSRC: u32 = 187;
pub(super) const R_SHEETSRC_END: u32 = 188;
pub(super) const R_SHARED: u32 = 189;
pub(super) const R_SHARED_END: u32 = 190;
pub(super) const R_ARRAY: u32 = 191;
pub(super) const R_ARRAY_END: u32 = 192;
pub(super) const R_GROUP: u32 = 219;
pub(super) const R_GROUP_END: u32 = 220;
pub(super) const R_GITEMS: u32 = 221;
pub(super) const R_GITEMS_END: u32 = 222;
pub(super) const R_RANGEPR: u32 = 223;
pub(super) const R_RANGEPR_END: u32 = 224;
pub(super) const R_DISCRETE: u32 = 225;
pub(super) const R_DISCRETE_END: u32 = 226;
pub(super) const R_DISCRETE_X: u32 = 26;

pub(super) const R_INDEX: u32 = 26;

pub(super) const I_MISSING: u32 = 20;
pub(super) const I_NUM: u32 = 21;
pub(super) const I_BOOL: u32 = 22;
pub(super) const I_ERR: u32 = 23;
pub(super) const I_STR: u32 = 24;
pub(super) const I_DATE: u32 = 25;
pub(super) const I_CALC: u32 = 31;

pub(super) const R_CALCS: u32 = 243;
pub(super) const R_CALCS_END: u32 = 244;
pub(super) const R_CALCMEM: u32 = 245;
pub(super) const R_CALCMEM_END: u32 = 246;
pub(super) const R_AREA: u32 = 247;
pub(super) const R_AREA_END: u32 = 248;
pub(super) const R_REFS: u32 = 249;
pub(super) const R_REFS_END: u32 = 250;
pub(super) const R_REF: u32 = 251;
pub(super) const R_REF_END: u32 = 252;
pub(super) const R_TUPLES: u32 = 253;
pub(super) const R_TUPLES_END: u32 = 254;
pub(super) const R_TUPLE: u32 = 255;
pub(super) const R_TUPLE_END: u32 = 256;
pub(super) const R_MEMBERS: u32 = 257;
pub(super) const R_MEMBERS_END: u32 = 258;
pub(super) const R_TMEMBER: u32 = 259;
pub(super) const R_TMEMBER_END: u32 = 260;
pub(super) const R_REF_X: u32 = 382;
pub(super) const R_REF_X_END: u32 = 383;

pub(super) const REC_BEGIN: u32 = 193;
pub(super) const REC_END: u32 = 194;
pub(super) const REC_ROW: u32 = 33;
pub(super) const REC_TYPED_ROW: u32 = 34;

pub(crate) const W_CACHES: u32 = 384;
pub(crate) const W_CACHES_END: u32 = 385;
pub(crate) const W_CACHE: u32 = 386;
pub(crate) const W_CACHE_END: u32 = 387;

pub(super) const T_DEF: u32 = 280;
pub(super) const T_ITEM: u32 = 282;
pub(super) const T_ITEM_END: u32 = 281;
pub(super) const T_ITEMS: u32 = 283;
pub(super) const T_ITEMS_END: u32 = 284;
pub(super) const T_FIELD: u32 = 285;
pub(super) const T_FIELD_END: u32 = 286;
pub(super) const T_FIELDS: u32 = 287;
pub(super) const T_FIELDS_END: u32 = 288;
pub(super) const T_PAGE: u32 = 289;
pub(super) const T_PAGE_END: u32 = 290;
pub(super) const T_PAGES: u32 = 291;
pub(super) const T_PAGES_END: u32 = 292;
pub(super) const T_DATA: u32 = 293;
pub(super) const T_DATA_END: u32 = 294;
pub(super) const T_DATAS: u32 = 295;
pub(super) const T_DATAS_END: u32 = 296;
pub(super) const T_LINE: u32 = 297;
pub(super) const T_LINE_END: u32 = 298;
pub(super) const T_ROWITEMS: u32 = 299;
pub(super) const T_ROWITEMS_END: u32 = 300;
pub(super) const T_COLITEMS: u32 = 301;
pub(super) const T_COLITEMS_END: u32 = 302;
pub(super) const T_ROWFIELDS: u32 = 309;
pub(super) const T_ROWFIELDS_END: u32 = 310;
pub(super) const T_COLFIELDS: u32 = 311;
pub(super) const T_COLFIELDS_END: u32 = 312;
pub(super) const T_LOC_END: u32 = 313;
pub(super) const T_LOC: u32 = 314;
pub(super) const T_DEF_END: u32 = 315;
pub(super) const T_LINE_X: u32 = 388;
pub(super) const T_LINE_X_END: u32 = 389;
pub(super) const T_SORT: u32 = 459;
pub(super) const T_SORT_END: u32 = 460;
pub(super) const T_STYLE: u32 = 513;

pub(super) const FRT_BEGIN: u32 = 35;
pub(super) const FRT_END: u32 = 36;

pub(super) const T_FILL: u32 = 1061;

pub(super) const CACHE_FRT: u32 = 1066;
pub(super) const CACHE_FRT_END: u32 = 1067;
pub(super) const CACHE_CLEAN: u32 = 5153;
pub(super) const TABLE_FRT: u32 = 1062;
pub(super) const TABLE_FRT_END: u32 = 1063;
pub(super) const TABLE_FRT2: u32 = 5000;
pub(super) const TABLE_FRT2_END: u32 = 5001;

pub(super) const PAGE_ALL: u32 = 0x0010_00FE;

pub(super) const SUBTOTAL_ATTRS: [&str; 11] = [
    "sumSubtotal",
    "countASubtotal",
    "avgSubtotal",
    "maxSubtotal",
    "minSubtotal",
    "productSubtotal",
    "countSubtotal",
    "stdDevSubtotal",
    "stdDevPSubtotal",
    "varSubtotal",
    "varPSubtotal",
];

pub(super) const ITEM_TYPES: [&str; 15] = [
    "data", "default", "sum", "countA", "avg", "max", "min", "product", "count", "stdDev",
    "stdDevP", "var", "varP", "grand", "blank",
];

pub(super) const SHOW_AS: [&str; 9] = [
    "normal",
    "difference",
    "percent",
    "percentDiff",
    "runTotal",
    "percentOfRow",
    "percentOfCol",
    "percentOfTotal",
    "index",
];

pub(super) const SUBTOTALS: [&str; 11] = [
    "sum",
    "count",
    "average",
    "max",
    "min",
    "product",
    "countNums",
    "stdDev",
    "stdDevp",
    "var",
    "varp",
];
