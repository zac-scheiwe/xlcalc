pub(crate) struct Grbit {
    pub name: &'static str,
    pub period: u8,
    pub head: &'static [u32],
}

pub(crate) static GRBIT: &[Grbit] = &[
    Grbit {
        name: "ABS",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ACCRINT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ACCRINTM",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ACOS",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ACOSH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ACOT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ACOTH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ADDRESS",
        period: 1,
        head: &[0x1, 0x81, 0x81, 0x84, 0x82],
    },
    Grbit {
        name: "AGGREGATE",
        period: 1,
        head: &[0x1, 0x1, 0x48, 0x9, 0x8],
    },
    Grbit {
        name: "AMORDEGRC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "AMORLINC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "AND",
        period: 1,
        head: &[0x4c],
    },
    Grbit {
        name: "ARABIC",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "AREAS",
        period: 1,
        head: &[0x8],
    },
    Grbit {
        name: "ARRAYTOTEXT",
        period: 1,
        head: &[0x40, 0x1],
    },
    Grbit {
        name: "ASC",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "ASIN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ASINH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ATAN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ATAN2",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ATANH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "AVEDEV",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "AVERAGE",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "AVERAGEA",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "AVERAGEIF",
        period: 2,
        head: &[0x8, 0x17],
    },
    Grbit {
        name: "AVERAGEIFS",
        period: 2,
        head: &[0x8, 0x8, 0x17],
    },
    Grbit {
        name: "BAHTTEXT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "BASE",
        period: 1,
        head: &[0x1, 0x1, 0x81],
    },
    Grbit {
        name: "BESSELI",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "BESSELJ",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "BESSELK",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "BESSELY",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "BETA.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4, 0x81],
    },
    Grbit {
        name: "BETA.INV",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x81],
    },
    Grbit {
        name: "BETADIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x81],
    },
    Grbit {
        name: "BETAINV",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x81],
    },
    Grbit {
        name: "BIN2DEC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "BIN2HEX",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "BIN2OCT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "BINOM.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "BINOM.DIST.RANGE",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x81],
    },
    Grbit {
        name: "BINOM.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "BINOMDIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "BITAND",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "BITLSHIFT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "BITOR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "BITRSHIFT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "BITXOR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CEILING",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CEILING.MATH",
        period: 1,
        head: &[0x1, 0x81],
    },
    Grbit {
        name: "CEILING.PRECISE",
        period: 1,
        head: &[0x1, 0x81],
    },
    Grbit {
        name: "CELL",
        period: 1,
        head: &[0x2, 0x8],
    },
    Grbit {
        name: "CHAR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CHIDIST",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CHIINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CHISQ.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x4],
    },
    Grbit {
        name: "CHISQ.DIST.RT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CHISQ.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CHISQ.INV.RT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CHISQ.TEST",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "CHITEST",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "CHOOSE",
        period: 1,
        head: &[0x41, 0x7f],
    },
    Grbit {
        name: "CHOOSECOLS",
        period: 1,
        head: &[0x48, 0x41],
    },
    Grbit {
        name: "CHOOSEROWS",
        period: 1,
        head: &[0x48, 0x41],
    },
    Grbit {
        name: "CLEAN",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "CODE",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "COLUMN",
        period: 1,
        head: &[0x8],
    },
    Grbit {
        name: "COLUMNS",
        period: 1,
        head: &[0x48],
    },
    Grbit {
        name: "COMBIN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "COMBINA",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "COMPLEX",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "CONCAT",
        period: 1,
        head: &[0xca],
    },
    Grbit {
        name: "CONCATENATE",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "CONFIDENCE",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CONFIDENCE.NORM",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CONFIDENCE.T",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CONVERT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "CORREL",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "COS",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "COSH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "COT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "COTH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "COUNT",
        period: 1,
        head: &[0x59],
    },
    Grbit {
        name: "COUNTA",
        period: 1,
        head: &[0xd9],
    },
    Grbit {
        name: "COUNTBLANK",
        period: 1,
        head: &[0x8],
    },
    Grbit {
        name: "COUNTIF",
        period: 1,
        head: &[0x8, 0x17],
    },
    Grbit {
        name: "COUNTIFS",
        period: 2,
        head: &[0x8, 0x17],
    },
    Grbit {
        name: "COUPDAYBS",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "COUPDAYS",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "COUPDAYSNC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "COUPNCD",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "COUPNUM",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "COUPPCD",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "COVAR",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "COVARIANCE.P",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "COVARIANCE.S",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "CRITBINOM",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CSC",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CSCH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "CUBEKPIMEMBER",
        period: 2,
        head: &[0x2, 0x2, 0x1],
    },
    Grbit {
        name: "CUBEMEMBER",
        period: 2,
        head: &[0x2, 0x4a],
    },
    Grbit {
        name: "CUBEMEMBERPROPERTY",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "CUBERANKEDMEMBER",
        period: 2,
        head: &[0x2, 0x2, 0x1],
    },
    Grbit {
        name: "CUBESET",
        period: 2,
        head: &[0x2, 0x4a, 0x2, 0x1],
    },
    Grbit {
        name: "CUBESETCOUNT",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "CUBEVALUE",
        period: 1,
        head: &[0x2, 0x4a],
    },
    Grbit {
        name: "CUMIPMT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "CUMPRINC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DATE",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "DATEDIF",
        period: 1,
        head: &[0x1, 0x1, 0x2],
    },
    Grbit {
        name: "DATESTRING",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "DATEVALUE",
        period: 1,
        head: &[0x3],
    },
    Grbit {
        name: "DAVERAGE",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DAY",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "DAYS",
        period: 1,
        head: &[0x3],
    },
    Grbit {
        name: "DAYS360",
        period: 1,
        head: &[0x1, 0x1, 0x4],
    },
    Grbit {
        name: "DB",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "DBCS",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "DCOUNT",
        period: 1,
        head: &[0x8, 0x8b, 0xa],
    },
    Grbit {
        name: "DCOUNTA",
        period: 1,
        head: &[0x8, 0x8b, 0xa],
    },
    Grbit {
        name: "DDB",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "DEC2BIN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DEC2HEX",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DEC2OCT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DECIMAL",
        period: 1,
        head: &[0x2, 0x1],
    },
    Grbit {
        name: "DEGREES",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "DELTA",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DEVSQ",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "DGET",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DISC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DMAX",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DMIN",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DOLLAR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "DOLLARDE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DOLLARFR",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DPRODUCT",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DROP",
        period: 1,
        head: &[0x48, 0x81],
    },
    Grbit {
        name: "DSTDEV",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DSTDEVP",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DSUM",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DURATION",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "DVAR",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "DVARP",
        period: 1,
        head: &[0x8, 0xb, 0xa],
    },
    Grbit {
        name: "EDATE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "EFFECT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ENCODEURL",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "EOMONTH",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ERF",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ERF.PRECISE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ERFC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ERFC.PRECISE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ERROR.TYPE",
        period: 1,
        head: &[0x17],
    },
    Grbit {
        name: "EVEN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "EXACT",
        period: 1,
        head: &[0x800002],
    },
    Grbit {
        name: "EXP",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "EXPAND",
        period: 1,
        head: &[0x140, 0x81, 0x81, 0x8001d7],
    },
    Grbit {
        name: "EXPON.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x4],
    },
    Grbit {
        name: "EXPONDIST",
        period: 1,
        head: &[0x1, 0x1, 0x4],
    },
    Grbit {
        name: "F.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "F.DIST.RT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "F.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "F.INV.RT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "F.TEST",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "FACT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "FACTDOUBLE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "FDIST",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "FIELDVALUE",
        period: 1,
        head: &[0x800017, 0x2],
    },
    Grbit {
        name: "FILTER",
        period: 1,
        head: &[0x48, 0x48, 0xd7],
    },
    Grbit {
        name: "FILTERXML",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "FIND",
        period: 1,
        head: &[0x2, 0x2, 0x1],
    },
    Grbit {
        name: "FINDB",
        period: 1,
        head: &[0x2, 0x2, 0x1],
    },
    Grbit {
        name: "FINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "FISHER",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "FISHERINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "FIXED",
        period: 1,
        head: &[0x1, 0x81, 0x4],
    },
    Grbit {
        name: "FLOOR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "FLOOR.MATH",
        period: 1,
        head: &[0x1, 0x81],
    },
    Grbit {
        name: "FLOOR.PRECISE",
        period: 1,
        head: &[0x1, 0x81],
    },
    Grbit {
        name: "FORECAST",
        period: 1,
        head: &[0x1, 0x40],
    },
    Grbit {
        name: "FORECAST.ETS",
        period: 1,
        head: &[0x1, 0x48, 0x48, 0x81],
    },
    Grbit {
        name: "FORECAST.ETS.CONFINT",
        period: 1,
        head: &[0x1, 0x48, 0x48, 0x81],
    },
    Grbit {
        name: "FORECAST.ETS.SEASONALITY",
        period: 1,
        head: &[0x48, 0x48, 0x81],
    },
    Grbit {
        name: "FORECAST.ETS.STAT",
        period: 1,
        head: &[0x48, 0x48, 0x1, 0x81],
    },
    Grbit {
        name: "FORECAST.LINEAR",
        period: 1,
        head: &[0x1, 0x40],
    },
    Grbit {
        name: "FORMULATEXT",
        period: 1,
        head: &[0x8],
    },
    Grbit {
        name: "FREQUENCY",
        period: 1,
        head: &[0x48],
    },
    Grbit {
        name: "FTEST",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "FV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "FVSCHEDULE",
        period: 1,
        head: &[0x9f, 0xdf],
    },
    Grbit {
        name: "GAMMA",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "GAMMA.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "GAMMA.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "GAMMADIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "GAMMAINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "GAMMALN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "GAMMALN.PRECISE",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "GAUSS",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "GCD",
        period: 1,
        head: &[0xdf],
    },
    Grbit {
        name: "GEOMEAN",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "GESTEP",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "GETPIVOTDATA",
        period: 2,
        head: &[0xa, 0xa, 0x2, 0x97],
    },
    Grbit {
        name: "GROWTH",
        period: 1,
        head: &[0x48, 0xc8, 0xc8, 0x8c],
    },
    Grbit {
        name: "HARMEAN",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "HEX2BIN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "HEX2DEC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "HEX2OCT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "HLOOKUP",
        period: 1,
        head: &[0x800007, 0x49, 0x49, 0x4],
    },
    Grbit {
        name: "HOUR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "HSTACK",
        period: 1,
        head: &[0x48],
    },
    Grbit {
        name: "HYPERLINK",
        period: 1,
        head: &[0x2, 0x47],
    },
    Grbit {
        name: "HYPGEOM.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "HYPGEOMDIST",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "IF",
        period: 1,
        head: &[0x44, 0x80007f],
    },
    Grbit {
        name: "IFERROR",
        period: 1,
        head: &[0x57, 0x7f],
    },
    Grbit {
        name: "IFNA",
        period: 1,
        head: &[0x57, 0x7f],
    },
    Grbit {
        name: "IFS",
        period: 2,
        head: &[0x54, 0x7f],
    },
    Grbit {
        name: "IMABS",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMAGE",
        period: 1,
        head: &[0x2, 0x82, 0x81],
    },
    Grbit {
        name: "IMAGINARY",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMARGUMENT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMCONJUGATE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMCOS",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMCOSH",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMCOT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMCSC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMCSCH",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMDIV",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMEXP",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMLN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMLOG10",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMLOG2",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMPOWER",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMPRODUCT",
        period: 1,
        head: &[0xdf],
    },
    Grbit {
        name: "IMREAL",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMSEC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMSECH",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMSIN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMSINH",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMSQRT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMSUB",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IMSUM",
        period: 1,
        head: &[0xdf],
    },
    Grbit {
        name: "IMTAN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "INDEX",
        period: 1,
        head: &[0x48, 0x1],
    },
    Grbit {
        name: "INDIRECT",
        period: 1,
        head: &[0x2, 0x4],
    },
    Grbit {
        name: "INFO",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "INT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "INTERCEPT",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "INTRATE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "IPMT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "IRR",
        period: 1,
        head: &[0x48, 0x1],
    },
    Grbit {
        name: "ISBLANK",
        period: 1,
        head: &[0x117],
    },
    Grbit {
        name: "ISERR",
        period: 1,
        head: &[0x17],
    },
    Grbit {
        name: "ISERROR",
        period: 1,
        head: &[0x17],
    },
    Grbit {
        name: "ISEVEN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ISFORMULA",
        period: 1,
        head: &[0x8],
    },
    Grbit {
        name: "ISLOGICAL",
        period: 1,
        head: &[0x117],
    },
    Grbit {
        name: "ISNA",
        period: 1,
        head: &[0x17],
    },
    Grbit {
        name: "ISNONTEXT",
        period: 1,
        head: &[0x117],
    },
    Grbit {
        name: "ISNUMBER",
        period: 1,
        head: &[0x117],
    },
    Grbit {
        name: "ISO.CEILING",
        period: 1,
        head: &[0x1, 0x81],
    },
    Grbit {
        name: "ISODD",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ISOMITTED",
        period: 1,
        head: &[0x8003df],
    },
    Grbit {
        name: "ISOWEEKNUM",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ISPMT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ISREF",
        period: 1,
        head: &[0x5f],
    },
    Grbit {
        name: "ISTEXT",
        period: 1,
        head: &[0x117],
    },
    Grbit {
        name: "ISTHAIDIGIT",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "KURT",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "LARGE",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "LCM",
        period: 1,
        head: &[0xdf],
    },
    Grbit {
        name: "LEFT",
        period: 1,
        head: &[0x2, 0x11],
    },
    Grbit {
        name: "LEFTB",
        period: 1,
        head: &[0x2, 0x11],
    },
    Grbit {
        name: "LEN",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "LENB",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "LINEST",
        period: 1,
        head: &[0x48, 0xc8, 0x8c],
    },
    Grbit {
        name: "LN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "LOG",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "LOG10",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "LOGEST",
        period: 1,
        head: &[0x48, 0xc8, 0x8c],
    },
    Grbit {
        name: "LOGINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "LOGNORM.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "LOGNORM.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "LOGNORMDIST",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "LOOKUP",
        period: 1,
        head: &[0x800007, 0x48],
    },
    Grbit {
        name: "LOWER",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "MATCH",
        period: 1,
        head: &[0x800007, 0x800049, 0x9],
    },
    Grbit {
        name: "MAX",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "MAXA",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "MAXIFS",
        period: 2,
        head: &[0x8, 0x8, 0x17],
    },
    Grbit {
        name: "MDETERM",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "MDURATION",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "MEDIAN",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "MID",
        period: 1,
        head: &[0x2, 0x1],
    },
    Grbit {
        name: "MIDB",
        period: 1,
        head: &[0x2, 0x1],
    },
    Grbit {
        name: "MIN",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "MINA",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "MINIFS",
        period: 2,
        head: &[0x8, 0x8, 0x17],
    },
    Grbit {
        name: "MINUTE",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "MINVERSE",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "MIRR",
        period: 1,
        head: &[0x48, 0x1],
    },
    Grbit {
        name: "MMULT",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "MOD",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "MODE",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "MODE.MULT",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "MODE.SNGL",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "MONTH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "MROUND",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "MULTINOMIAL",
        period: 1,
        head: &[0xdf],
    },
    Grbit {
        name: "MUNIT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "N",
        period: 1,
        head: &[0xf],
    },
    Grbit {
        name: "NEGBINOM.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "NEGBINOMDIST",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NETWORKDAYS",
        period: 1,
        head: &[0x9f, 0x9f, 0xdf],
    },
    Grbit {
        name: "NETWORKDAYS.INTL",
        period: 1,
        head: &[0x9f, 0x9f, 0x93, 0xdf],
    },
    Grbit {
        name: "NOMINAL",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "NORM.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "NORM.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NORM.S.DIST",
        period: 1,
        head: &[0x1, 0x4],
    },
    Grbit {
        name: "NORM.S.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NORMDIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "NORMINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NORMSDIST",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NORMSINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NOT",
        period: 1,
        head: &[0x4],
    },
    Grbit {
        name: "NPER",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NPV",
        period: 1,
        head: &[0x1, 0x49],
    },
    Grbit {
        name: "NUMBERSTRING",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "NUMBERVALUE",
        period: 1,
        head: &[0x2, 0x82],
    },
    Grbit {
        name: "OCT2BIN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "OCT2DEC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "OCT2HEX",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ODD",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ODDFPRICE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ODDFYIELD",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ODDLPRICE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "ODDLYIELD",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "OFFSET",
        period: 1,
        head: &[0x8, 0x81],
    },
    Grbit {
        name: "OR",
        period: 1,
        head: &[0x4c],
    },
    Grbit {
        name: "PDURATION",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "PEARSON",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "PERCENTILE",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "PERCENTILE.EXC",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "PERCENTILE.INC",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "PERCENTRANK",
        period: 1,
        head: &[0x49, 0x1, 0x81],
    },
    Grbit {
        name: "PERCENTRANK.EXC",
        period: 1,
        head: &[0x49, 0x1, 0x81],
    },
    Grbit {
        name: "PERCENTRANK.INC",
        period: 1,
        head: &[0x49, 0x1, 0x81],
    },
    Grbit {
        name: "PERMUT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "PERMUTATIONA",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "PHI",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "PHONETIC",
        period: 1,
        head: &[0x8],
    },
    Grbit {
        name: "PMT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "POISSON",
        period: 1,
        head: &[0x1, 0x1, 0x4],
    },
    Grbit {
        name: "POISSON.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x4],
    },
    Grbit {
        name: "POWER",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "PPMT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "PRICE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "PRICEDISC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "PRICEMAT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "PROB",
        period: 1,
        head: &[0x40, 0x40, 0x1, 0x81],
    },
    Grbit {
        name: "PRODUCT",
        period: 1,
        head: &[0xc9],
    },
    Grbit {
        name: "PROPER",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "PV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "QUARTILE",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "QUARTILE.EXC",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "QUARTILE.INC",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "QUOTIENT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "RADIANS",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "RANDARRAY",
        period: 1,
        head: &[0x81, 0x81, 0x81, 0x81, 0x84],
    },
    Grbit {
        name: "RANDBETWEEN",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "RANK",
        period: 1,
        head: &[0x1, 0x8, 0x4],
    },
    Grbit {
        name: "RANK.AVG",
        period: 1,
        head: &[0x1, 0x8, 0x4],
    },
    Grbit {
        name: "RANK.EQ",
        period: 1,
        head: &[0x1, 0x8, 0x4],
    },
    Grbit {
        name: "RATE",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "RECEIVED",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "REPLACE",
        period: 1,
        head: &[0x2, 0x1, 0x1, 0x2],
    },
    Grbit {
        name: "REPLACEB",
        period: 1,
        head: &[0x2, 0x1, 0x1, 0x2],
    },
    Grbit {
        name: "REPT",
        period: 1,
        head: &[0x2, 0x1],
    },
    Grbit {
        name: "RIGHT",
        period: 1,
        head: &[0x2, 0x11],
    },
    Grbit {
        name: "RIGHTB",
        period: 1,
        head: &[0x2, 0x11],
    },
    Grbit {
        name: "ROMAN",
        period: 1,
        head: &[0x1, 0x5],
    },
    Grbit {
        name: "ROUND",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ROUNDBAHTDOWN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ROUNDBAHTUP",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ROUNDDOWN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ROUNDUP",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "ROW",
        period: 1,
        head: &[0x8],
    },
    Grbit {
        name: "ROWS",
        period: 1,
        head: &[0x48],
    },
    Grbit {
        name: "RRI",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "RSQ",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "SEARCH",
        period: 1,
        head: &[0x2, 0x2, 0x1],
    },
    Grbit {
        name: "SEARCHB",
        period: 1,
        head: &[0x2, 0x2, 0x1],
    },
    Grbit {
        name: "SEC",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SECH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SECOND",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SEQUENCE",
        period: 1,
        head: &[0x81],
    },
    Grbit {
        name: "SERIESSUM",
        period: 1,
        head: &[0x9f, 0x9f, 0x9f, 0xdf],
    },
    Grbit {
        name: "SHEET",
        period: 1,
        head: &[0x4a],
    },
    Grbit {
        name: "SHEETS",
        period: 1,
        head: &[0x48],
    },
    Grbit {
        name: "SIGN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SIN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SINGLE",
        period: 1,
        head: &[0x8003cf],
    },
    Grbit {
        name: "SINH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SKEW",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "SKEW.P",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "SLN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SLOPE",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "SMALL",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "SORT",
        period: 1,
        head: &[0x140, 0xc0, 0xc0, 0x4],
    },
    Grbit {
        name: "SORTBY",
        period: 2,
        head: &[0x140, 0x140, 0x81],
    },
    Grbit {
        name: "SQRT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "SQRTPI",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "STANDARDIZE",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "STDEV",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "STDEV.P",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "STDEV.S",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "STDEVA",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "STDEVP",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "STDEVPA",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "STEYX",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "STOCKHISTORY",
        period: 1,
        head: &[0x800002, 0x1, 0x81],
    },
    Grbit {
        name: "SUBSTITUTE",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "SUBTOTAL",
        period: 1,
        head: &[0x1, 0x8],
    },
    Grbit {
        name: "SUM",
        period: 1,
        head: &[0x800049],
    },
    Grbit {
        name: "SUMIF",
        period: 2,
        head: &[0x8, 0x17],
    },
    Grbit {
        name: "SUMIFS",
        period: 2,
        head: &[0x8, 0x8, 0x17],
    },
    Grbit {
        name: "SUMPRODUCT",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "SUMSQ",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "SUMX2MY2",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "SUMX2PY2",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "SUMXMY2",
        period: 1,
        head: &[0x40],
    },
    Grbit {
        name: "SWITCH",
        period: 1,
        head: &[0x57, 0x7f],
    },
    Grbit {
        name: "SYD",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "T",
        period: 1,
        head: &[0xf],
    },
    Grbit {
        name: "T.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x4],
    },
    Grbit {
        name: "T.DIST.2T",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "T.DIST.RT",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "T.INV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "T.INV.2T",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "T.TEST",
        period: 1,
        head: &[0x40, 0x40, 0x1],
    },
    Grbit {
        name: "TAKE",
        period: 1,
        head: &[0x48, 0x81],
    },
    Grbit {
        name: "TAN",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "TANH",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "TBILLEQ",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "TBILLPRICE",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "TBILLYIELD",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "TDIST",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "TEXT",
        period: 1,
        head: &[0x800007, 0x2],
    },
    Grbit {
        name: "TEXTAFTER",
        period: 1,
        head: &[0x2, 0x42, 0x81, 0x81, 0x81, 0x92],
    },
    Grbit {
        name: "TEXTBEFORE",
        period: 1,
        head: &[0x2, 0x42, 0x81, 0x81, 0x81, 0x92],
    },
    Grbit {
        name: "TEXTJOIN",
        period: 1,
        head: &[0xca, 0x84, 0xca],
    },
    Grbit {
        name: "TEXTSPLIT",
        period: 1,
        head: &[0x2, 0xc2, 0xc2, 0x84, 0x81, 0x8001d7],
    },
    Grbit {
        name: "THAIDAYOFWEEK",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "THAIDIGIT",
        period: 1,
        head: &[0x3],
    },
    Grbit {
        name: "THAIMONTHOFYEAR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "THAINUMSOUND",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "THAINUMSTRING",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "THAISTRINGLENGTH",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "THAIYEAR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "TIME",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "TIMEVALUE",
        period: 1,
        head: &[0x3],
    },
    Grbit {
        name: "TINV",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "TOCOL",
        period: 1,
        head: &[0x48, 0x81, 0x84],
    },
    Grbit {
        name: "TOROW",
        period: 1,
        head: &[0x48, 0x81, 0x84],
    },
    Grbit {
        name: "TRANSPOSE",
        period: 1,
        head: &[0x47],
    },
    Grbit {
        name: "TREND",
        period: 1,
        head: &[0x48, 0xc8, 0xc8, 0x8c],
    },
    Grbit {
        name: "TRIM",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "TRIMMEAN",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "TRUNC",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "TTEST",
        period: 1,
        head: &[0x40, 0x40, 0x1],
    },
    Grbit {
        name: "TYPE",
        period: 1,
        head: &[0x57],
    },
    Grbit {
        name: "UNICHAR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "UNICODE",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "UNIQUE",
        period: 1,
        head: &[0x140, 0x4],
    },
    Grbit {
        name: "UPPER",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "USDOLLAR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "VALUE",
        period: 1,
        head: &[0x7],
    },
    Grbit {
        name: "VALUETOTEXT",
        period: 1,
        head: &[0x800117, 0x1],
    },
    Grbit {
        name: "VAR",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "VAR.P",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "VAR.S",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "VARA",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "VARP",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "VARPA",
        period: 1,
        head: &[0x49],
    },
    Grbit {
        name: "VDB",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "VLOOKUP",
        period: 1,
        head: &[0x800007, 0x49, 0x49, 0x4],
    },
    Grbit {
        name: "VSTACK",
        period: 1,
        head: &[0x48],
    },
    Grbit {
        name: "WEBSERVICE",
        period: 1,
        head: &[0x2],
    },
    Grbit {
        name: "WEEKDAY",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "WEEKNUM",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "WEIBULL",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "WEIBULL.DIST",
        period: 1,
        head: &[0x1, 0x1, 0x1, 0x4],
    },
    Grbit {
        name: "WORKDAY",
        period: 1,
        head: &[0x9f, 0x9f, 0xdf],
    },
    Grbit {
        name: "WORKDAY.INTL",
        period: 1,
        head: &[0x9f, 0x9f, 0x93, 0xdf],
    },
    Grbit {
        name: "WRAPCOLS",
        period: 1,
        head: &[0x40, 0x1, 0x800197],
    },
    Grbit {
        name: "WRAPROWS",
        period: 1,
        head: &[0x40, 0x1, 0x800197],
    },
    Grbit {
        name: "XIRR",
        period: 1,
        head: &[0xdf, 0xdf, 0x9f],
    },
    Grbit {
        name: "XLOOKUP",
        period: 1,
        head: &[0x800107, 0x48, 0x48, 0x8001df, 0x81],
    },
    Grbit {
        name: "XMATCH",
        period: 1,
        head: &[0x800107, 0x48, 0x81],
    },
    Grbit {
        name: "XNPV",
        period: 1,
        head: &[0x9f, 0xdf],
    },
    Grbit {
        name: "XOR",
        period: 1,
        head: &[0x4c],
    },
    Grbit {
        name: "YEAR",
        period: 1,
        head: &[0x1],
    },
    Grbit {
        name: "YEARFRAC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "YIELD",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "YIELDDISC",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "YIELDMAT",
        period: 1,
        head: &[0x9f],
    },
    Grbit {
        name: "Z.TEST",
        period: 1,
        head: &[0x49, 0x1],
    },
    Grbit {
        name: "ZTEST",
        period: 1,
        head: &[0x49, 0x1],
    },
];
