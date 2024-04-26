use super::*;
use pretty_assertions::assert_eq;

#[ignore = "Not implemented"]
#[test]
fn return_types() {
    let (_, bag) = AST::from_str(
        "

        block main() => (out: bool) {
            out <- 10;
        }

    ",
    );
    let m_bag = bag.borrow_mut();
    m_bag.print();
    assert_eq!(m_bag.error_count(), 1);
}

#[ignore = "Not implemented"]
#[test]
fn assignment_types() {
    let (_, bag) = AST::from_str(
        "
        block main() => (out: bool) {
            a: int <- false;
            out <- a;
        }
        ",
    );
    let m_bag = bag.borrow_mut();
    m_bag.print();
    assert_eq!(m_bag.error_count(), 2);
}

#[ignore = "Not implemented"]
#[test]
fn expression_types() {
    let (_, bag) = AST::from_str(
        "
    block main() => (out: int) {
        b: int <- 5 + false * 10;
        out <- b;
    }
    ",
    );
    let m_bag = bag.borrow_mut();
    m_bag.print();
    assert!(m_bag.error_count() == 1);
}
