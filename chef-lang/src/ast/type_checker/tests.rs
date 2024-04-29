use super::*;

macro_rules! disallow {
    {$code:tt} => {
        let (_, bag) = AST::from_str($code);
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert!(m_bag.error_count() > 0);
    };
}

macro_rules! allow {
    {$code:tt} => {
        let (_, bag) = AST::from_str($code);
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert_eq!(m_bag.error_count(), 0);
    };
}

#[test]
fn return_types() {
    disallow! {"

        block main() => (out: bool) {
            out <- 10;
        }

    "};
}

#[test]
fn assignment_types() {
    disallow! {"

        block main() => (out: bool) {
            a: int <- false;
            out <- a;
        }

    "};
}

#[test]
fn expression_types() {
    disallow! {"

    block main() => (out: int) {
        b: int <- 5 + false * 10;
        out <- b;
    }

    "};
}

#[test]
fn expression_var_types() {
    disallow! {"

    block main() => (out: int) {
        i: int <- -10;
        b: bool <- true;
        out <- b + i * 10;
    }

    "};
}

#[test]
fn var_block_def_types() {
    disallow! {"

    block main() => (out: int(made-up)) {
        out <- 42;
    }

    "};
}

#[test]
fn var_def_types() {
    disallow! {"

    block main() => (out: bool) {
        b: bool(made-up) <- false;
        out <- b;
    }

    "};
}

#[test]
fn combine_ints() {
    allow! {"

    block main() => (out: many) {
        a: int <- 42;
        b: int <- 12;
        out <- a @ b;
    }

    "};
}

#[test]
fn combine_bool() {
    allow! {"

    block main() => (out: many) {
        a: int <- 42;
        b: bool <- false;
        out <- a @ b;
    }

    "};
}
