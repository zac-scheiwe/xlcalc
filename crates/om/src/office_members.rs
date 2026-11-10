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
        name: "OfficeTheme",
        iid: Guid::parse("000c03a0-0000-0000-c000-000000000046"),
        dual: false,
        hidden: false,
        members: M_OFFICETHEME,
    },
    Interface {
        name: "TextRange2",

        iid: Guid::parse("00000000-0000-0000-c000-000000000a02"),
        dual: false,
        hidden: false,
        members: M_TEXTRANGE2,
    },
];

static M_TEXTRANGE2: &[Member] = &[
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m(
        "Characters",
        0x6002_0010,
        Get,
        Object("TextRange2"),
        &[o("Start", I4), o("Length", I4)],
    ),
    m("Count", 0x6002_0011, Get, I4, &[]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("Length", 0x6002_0012, Get, I4, &[]),
    m("Parent", 0x6002_0002, Get, Dispatch, &[]),
    m("Text", 0, Get, Bstr, &[]),
    m("Text", 0, Put, Void, &[p("", Bstr)]),
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
    m("Name", 3, Get, Bstr, &[]),
    m("Name", 3, Put, Void, &[p("", Bstr)]),
    m("Parent", 1, Get, Dispatch, &[]),
    m("Type", 5, Get, I4, &[]),
    m("Value", 0, Get, Variant, &[]),
    m("Value", 0, Put, Void, &[p("", Variant)]),
];

static M_DOCUMENTPROPERTIES: &[Member] = &[
    m("Application", 0x6002_000c, Get, Dispatch, &[]),
    m("Count", 4, Get, I4, &[]),
    m("Creator", 0x6002_000d, Get, I4, &[]),
    m(
        "Item",
        0,
        Get,
        Object("DocumentProperty"),
        &[p("Index", Variant)],
    ),
    m("Parent", 1, Get, Dispatch, &[]),
];

static M_OFFICETHEME: &[Member] = &[
    m("Application", 0x6002_0000, Get, Dispatch, &[]),
    m("Creator", 0x6002_0001, Get, I4, &[]),
    m("Parent", 0x6002_0002, Get, Dispatch, &[]),
];
