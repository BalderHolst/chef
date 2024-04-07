use crate::blueprint::{CoordSet, WIRE_RANGE};

use noisy_float::prelude::*;

pub(crate) fn is_in_range(p1: &CoordSet, p2: &CoordSet) -> bool {
    let dx = p2.0 - p1.0;
    let dy = p2.1 - p1.1;
    let dist = (dx * dx + dy * dy).sqrt();
    dist <= WIRE_RANGE
}

#[test]
#[rustfmt::skip]
fn test_is_in_range() {
    let wire_range = r64(WIRE_RANGE);
    assert!(is_in_range(&(r64(0.0), r64(0.0)), &(r64(0.0), wire_range)));
    assert!(is_in_range(&(r64(0.0), r64(0.0)), &(wire_range, r64(0.0))));
    assert!(!is_in_range(&(r64(0.0), r64(0.0)), &(wire_range, r64(1.0))));
    assert!(!is_in_range(&(r64(0.0), r64(0.0)), &(wire_range, r64(-1.0))));
    assert!(is_in_range(&(r64(0.0), wire_range), &(r64(0.0), r64(0.0))));
    assert!(is_in_range(&(wire_range, r64(0.0)), &(r64(0.0), r64(0.0))));
    assert!(!is_in_range(&(wire_range, r64(1.0)), &(r64(0.0), r64(0.0))));
    assert!(!is_in_range(&(wire_range, r64(-1.0)), &(r64(0.0), r64(0.0))));
}
