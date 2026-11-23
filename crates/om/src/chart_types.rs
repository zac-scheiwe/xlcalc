const TABLE: [(i32, i32, i32); 107] = [
    (51, 0, 3),
    (52, 1, 3),
    (53, 2, 3),
    (54, 43, -4100),
    (55, 44, -4100),
    (56, 45, -4100),
    (-4100, 42, -4100),
    (57, 3, 2),
    (58, 4, 2),
    (59, 5, 2),
    (60, 47, -4099),
    (61, 48, -4099),
    (62, 49, -4099),
    (66, 9, 4),
    (67, 11, 4),
    (63, 9, 4),
    (64, 11, 4),
    (65, 7, 4),
    (68, 14, 5),
    (5, 12, 5),
    (69, 15, 5),
    (70, 55, -4102),
    (71, 13, 5),
    (72, 18, -4169),
    (73, 18, -4169),
    (74, 20, -4169),
    (75, 20, -4169),
    (76, 26, 1),
    (77, 27, 1),
    (78, 58, -4098),
    (79, 59, -4098),
    (80, 29, -4120),
    (81, 31, -4151),
    (82, 32, -4151),
    (83, 60, -4103),
    (-4103, 60, -4103),
    (84, 61, -4103),
    (85, 62, -4103),
    (86, 63, -4103),
    (15, 23, -4169),
    (87, 23, -4169),
    (88, 35, 3),
    (89, 36, 3),
    (90, 64, 4),
    (91, 65, 4),
    (90, 0, 3),
    (91, 0, 3),
    (92, 43, -4100),
    (93, 44, -4100),
    (94, 45, -4100),
    (95, 47, -4099),
    (96, 48, -4099),
    (97, 49, -4099),
    (98, 42, -4100),
    (99, 43, -4100),
    (100, 44, -4100),
    (101, 45, -4100),
    (102, 47, -4099),
    (103, 48, -4099),
    (104, 49, -4099),
    (105, 42, -4100),
    (106, 43, -4100),
    (107, 44, -4100),
    (108, 45, -4100),
    (109, 47, -4099),
    (110, 48, -4099),
    (111, 49, -4099),
    (112, 42, -4100),
    (4, 7, 4),
    (-4101, 50, -4101),
    (-4102, 54, -4102),
    (-4169, 20, -4169),
    (-4098, 56, -4098),
    (1, 25, 1),
    (-4120, 28, -4120),
    (-4120, 28, 6),
    (-4151, 31, -4151),
    (117, 70, -1),
    (118, 71, -1),
    (119, 72, -1),
    (120, 73, -1),
    (121, 74, -1),
    (122, 75, -1),
    (123, 76, -1),
    (124, 77, -1),
    (125, 78, -1),
    (126, 79, -1),
    (127, 80, -1),
    (128, 81, -1),
    (129, 82, -1),
    (130, 83, -1),
    (140, 93, -1),
    (131, 84, -1),
    (132, 85, -1),
    (133, 86, -1),
    (134, 87, -1),
    (135, 88, -1),
    (136, 89, -1),
    (137, 90, -1),
    (138, 91, -1),
    (139, 92, -1),
    (113, 66, -1),
    (114, 67, -1),
    (115, 68, -1),
    (-4111, -1, 3),
    (116, 69, 3),
    (-2, -3, -1),
];

fn row(xl: i32) -> Option<(i32, i32, i32)> {
    TABLE.iter().find(|r| r.0 == xl).copied()
}

pub(crate) fn owc_type(xl: i32) -> Option<i32> {
    row(xl).map(|r| r.1)
}

pub(crate) fn is_3d(xl: i32) -> bool {
    owc_type(xl).is_some_and(|o| o == -2 || (42..=63).contains(&o))
}

pub(crate) fn is_pie_kind(xl: i32) -> bool {
    row(xl).is_some_and(|r| matches!(r.2, 5 | -4102 | -4120))
}

pub(crate) fn is_bubble(xl: i32) -> bool {
    owc_type(xl) == Some(23)
}

pub(crate) fn refuses_gap_depth(xl: i32) -> bool {
    owc_type(xl).is_some_and(|o| matches!(o, 13 | 14 | 23 | 24))
}

pub(crate) fn refuses_data_labels(xl: i32) -> bool {
    owc_type(xl).is_some_and(|o| (60..=63).contains(&o))
}

pub(crate) fn canonical(xl: i32) -> i32 {
    if xl == -4103 {
        83
    } else {
        xl
    }
}

pub(crate) fn is_3d_pie(xl: i32) -> bool {
    owc_type(xl).is_some_and(|o| matches!(o, 54 | 55))
}

pub(crate) fn chart_type_put_error(xl: i32, series: usize) -> Option<u32> {
    let Some(owc) = owc_type(xl) else {
        return Some(0x8000_4005);
    };
    if !(0..=0x41).contains(&owc) {
        return Some(0x8007_0057);
    }
    let n = series as i64;
    match owc {
        0x23 if n != 3 => Some(0xB0D7_018E),
        0x24 if n != 4 => Some(0xB0D7_0190),
        0x3c..=0x3f if n < 2 => Some(0xB0D7_0192),
        0x40 if n != 4 => Some(0xB0D7_018F),
        0x41 if n != 5 => Some(0xB0D7_0191),
        _ => None,
    }
}

pub(crate) fn series_type_put_error(xl: i32) -> Option<u32> {
    let Some(owc) = owc_type(xl) else {
        return Some(0x8000_4005);
    };
    if !(0..=0x41).contains(&owc) {
        return Some(0x8007_0057);
    }
    matches!(owc, 0x24 | 0x40 | 0x41).then_some(0xB0D7_0194)
}

fn label_position_bit(xl_pos: i32) -> Option<u32> {
    Some(match xl_pos {
        -4108 => 1,
        4 => 3,
        3 => 2,
        2 => 4,
        -4131 => 5,
        -4152 => 6,
        0 => 7,
        1 => 8,
        5 => 9,
        _ => return None,
    })
}

fn valid_label_positions(xl: i32) -> u32 {
    let Some(owc) = owc_type(xl) else { return 1 };
    if is_pie_kind(xl) {
        return if row(xl).is_some_and(|r| r.2 == -4120) {
            1
        } else {
            0x217
        };
    }
    if is_3d(xl) {
        return 1;
    }
    match owc {
        1 | 2 | 4 | 5 => 0xf,
        0 | 3 => 0x1f,
        7..=11 | 18..=20 | 23 => 0x1e3,
        _ => 1,
    }
}

pub(crate) fn label_position_valid(xl: i32, xl_pos: i32) -> bool {
    label_position_bit(xl_pos).is_some_and(|b| valid_label_positions(xl) >> b & 1 == 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn surface_and_stock() {
        assert_eq!(owc_type(-4103), Some(60));
        assert_eq!(chart_type_put_error(-4103, 3), None);
        assert_eq!(chart_type_put_error(83, 1), Some(0xB0D7_0192));
        assert_eq!(chart_type_put_error(88, 2), Some(0xB0D7_018E));
        assert_eq!(chart_type_put_error(7, 3), Some(0x8000_4005));
        assert!(is_3d(83) && !is_3d(51) && is_pie_kind(-4120) && is_bubble(87));
        assert!(refuses_gap_depth(15) && !refuses_gap_depth(5));
    }
}
