use crate::typelib::InvokeKind::*;
use crate::typelib::Ty::*;
use crate::typelib::{h, m, o, p, Guid, Interface, Member};

pub static INTERFACES: &[Interface] = &[
    Interface {
        name: "COMAddIn",
        iid: Guid::parse("000c033a-0000-0000-c000-000000000046"),
        dual: false,
        hidden: false,
        members: M_COMADDIN,
    },
    Interface {
        name: "COMAddIns",
        iid: Guid::parse("000c0339-0000-0000-c000-000000000046"),
        dual: false,
        hidden: false,
        members: M_COMADDINS,
    },
    Interface {
        name: "DocumentProperties",
        iid: Guid::parse("2df8d04d-5bfa-101b-bde5-00aa0044de52"),
        dual: false,
        hidden: false,
        members: M_DOCUMENTPROPERTIES,
    },
    Interface {
        name: "DocumentProperty",
        iid: Guid::parse("2df8d04e-5bfa-101b-bde5-00aa0044de52"),
        dual: false,
        hidden: false,
        members: M_DOCUMENTPROPERTY,
    },
    Interface {
        name: "Font2",
        iid: Guid::parse("000c039a-0000-0000-c000-000000000046"),
        dual: false,
        hidden: false,
        members: M_FONT2,
    },
    Interface {
        name: "OfficeTheme",
        iid: Guid::parse("000c03a0-0000-0000-c000-000000000046"),
        dual: false,
        hidden: false,
        members: M_OFFICETHEME,
    },
    Interface {
        name: "ParagraphFormat2",
        iid: Guid::parse("000c0399-0000-0000-c000-000000000046"),
        dual: false,
        hidden: false,
        members: M_PARAGRAPHFORMAT2,
    },
    Interface {
        name: "TextRange2",
        iid: Guid::parse("000c0397-0000-0000-c000-000000000046"),
        dual: false,
        hidden: false,
        members: M_TEXTRANGE2,
    },
];

static M_TEXTRANGE2: &[Member] = &[
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m(
        "Characters",
        7,
        Get,
        Object("TextRange2"),
        &[o("Start", I4), o("Length", I4)],
    ),
    m("Count", 1, Get, I4, &[]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("Font", 0xb, Get, Object("Font2"), &[]),
    m("Length", 0xc, Get, I4, &[]),
    m("ParagraphFormat", 0xa, Get, Object("ParagraphFormat2"), &[]),
    m(
        "Paragraphs",
        4,
        Get,
        Object("TextRange2"),
        &[o("Start", I4), o("Length", I4)],
    ),
    m("Parent", 3, Get, Dispatch, &[]),
    m("Start", 0xd, Get, I4, &[]),
    m("Text", 0, Get, Bstr, &[]),
    m("Text", 0, Put, Void, &[p("", Bstr)]),
];

static M_FONT2: &[Member] = &[
    m("Allcaps", 0xc, Get, Enum("MsoTriState"), &[]),
    m("Allcaps", 0xc, Put, Void, &[p("", Enum("MsoTriState"))]),
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m("BaselineOffset", 7, Get, R4, &[]),
    m("BaselineOffset", 7, Put, Void, &[p("", R4)]),
    m("Bold", 2, Get, Enum("MsoTriState"), &[]),
    m("Bold", 2, Put, Void, &[p("", Enum("MsoTriState"))]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("DoubleStrikeThrough", 0xd, Get, Enum("MsoTriState"), &[]),
    m(
        "DoubleStrikeThrough",
        0xd,
        Put,
        Void,
        &[p("", Enum("MsoTriState"))],
    ),
    m("Fill", 0xf, Get, Object("FillFormat"), &[]),
    m("Italic", 3, Get, Enum("MsoTriState"), &[]),
    m("Italic", 3, Put, Void, &[p("", Enum("MsoTriState"))]),
    m("Kerning", 8, Get, R4, &[]),
    m("Kerning", 8, Put, Void, &[p("", R4)]),
    m("Name", 0x1e, Get, Bstr, &[]),
    m("Name", 0x1e, Put, Void, &[p("", Bstr)]),
    m("Parent", 1, Get, Dispatch, &[]),
    m("Size", 9, Get, R4, &[]),
    m("Size", 9, Put, Void, &[p("", R4)]),
    m("Smallcaps", 0x16, Get, Enum("MsoTriState"), &[]),
    m("Smallcaps", 0x16, Put, Void, &[p("", Enum("MsoTriState"))]),
    m("Spacing", 0xa, Get, R4, &[]),
    m("Spacing", 0xa, Put, Void, &[p("", R4)]),
    m("Strike", 4, Get, Enum("MsoTextStrike"), &[]),
    m("Strike", 4, Put, Void, &[p("", Enum("MsoTextStrike"))]),
    m("StrikeThrough", 0x18, Get, Enum("MsoTriState"), &[]),
    m(
        "StrikeThrough",
        0x18,
        Put,
        Void,
        &[p("", Enum("MsoTriState"))],
    ),
    m("Subscript", 0x19, Get, Enum("MsoTriState"), &[]),
    m("Subscript", 0x19, Put, Void, &[p("", Enum("MsoTriState"))]),
    m("Superscript", 0x1a, Get, Enum("MsoTriState"), &[]),
    m(
        "Superscript",
        0x1a,
        Put,
        Void,
        &[p("", Enum("MsoTriState"))],
    ),
    m(
        "UnderlineStyle",
        0xb,
        Get,
        Enum("MsoTextUnderlineType"),
        &[],
    ),
    m(
        "UnderlineStyle",
        0xb,
        Put,
        Void,
        &[p("", Enum("MsoTextUnderlineType"))],
    ),
];

static M_PARAGRAPHFORMAT2: &[Member] = &[
    m("Alignment", 2, Get, Enum("MsoParagraphAlignment"), &[]),
    m(
        "Alignment",
        2,
        Put,
        Void,
        &[p("", Enum("MsoParagraphAlignment"))],
    ),
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("Parent", 1, Get, Dispatch, &[]),
];

static M_COMADDIN: &[Member] = &[
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m("Connect", 6, Get, Bool, &[]),
    m("Connect", 6, Put, Void, &[p("", Bool)]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("Description", 0, Get, Bstr, &[]),
    m("Description", 0, Put, Void, &[p("", Bstr)]),
    m("Guid", 4, Get, Bstr, &[]),
    m("Object", 7, Get, Dispatch, &[]),
    m("Object", 7, Put, Void, &[p("", Dispatch)]),
    m("Parent", 8, Get, Dispatch, &[]),
    m("ProgId", 3, Get, Bstr, &[]),
];

static M_COMADDINS: &[Member] = &[
    h("_NewEnum", -4, Get, Unknown, &[]),
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m("Count", 1, Get, I4, &[]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("Item", 0, Func, Object("COMAddIn"), &[p("Index", Variant)]),
    m("Parent", 3, Get, Dispatch, &[]),
    h("SetAppModal", 4, Func, Void, &[p("varfModal", Bool)]),
    m("Update", 2, Func, Void, &[]),
];

static M_DOCUMENTPROPERTY: &[Member] = &[
    m("Application", 0x6002_000c, Get, Dispatch, &[]),
    m("Delete", 0x6002_0010, Func, Void, &[]),
    m("Name", 3, Get, Bstr, &[]),
    m("Name", 3, Put, Void, &[p("", Bstr)]),
    m("Parent", 1, Get, Dispatch, &[]),
    m("Type", 5, Get, I4, &[]),
    m("Value", 0, Get, Variant, &[]),
    m("Value", 0, Put, Void, &[p("", Variant)]),
];

static M_DOCUMENTPROPERTIES: &[Member] = &[
    m(
        "Add",
        0x6002_000e,
        Func,
        Object("DocumentProperty"),
        &[
            p("Name", Variant),
            p("LinkToContent", Variant),
            p("Type", Variant),
            p("Value", Variant),
            o("LinkSource", Variant),
        ],
    ),
    m("Application", 0x6002_000c, Get, Dispatch, &[]),
    m("Count", 4, Get, I4, &[]),
    m("Creator", 0x6002_000d, Get, I4, &[]),
    m(
        "Item",
        0,
        Get,
        Object("DocumentProperty"),
        &[o("Index", Variant)],
    ),
    m("Parent", 1, Get, Dispatch, &[]),
];

static M_OFFICETHEME: &[Member] = &[
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("Parent", 0x6002_0002, Get, Dispatch, &[]),
];
