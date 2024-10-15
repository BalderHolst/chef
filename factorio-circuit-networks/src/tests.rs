use std::cmp::{self, Ordering};

use super::*;
use factorio_blueprint::objects::{self as fbo, EntityNumber, OneBasedIndex};

fn roundtrip(entity: Entity) {
    let mut entity1 = entity.clone();
    let fbo_entity = fbo::Entity::from(entity.clone());
    let mut entity2 = Entity::from(fbo_entity);

    type C = (OneBasedIndex, EntityNumber, i32, WireColor);
    fn ord(this: &C, that: &C) -> Ordering {
        cmp::Ord::cmp(&this.0, &that.0)
            .then_with(|| cmp::Ord::cmp(&this.1, &that.1))
            .then_with(|| cmp::Ord::cmp(&this.2, &that.2))
            .then_with(|| (this.3 as usize).cmp(&(that.3 as usize)))
    }

    entity1.connections.sort_by(ord);
    entity2.connections.sort_by(ord);

    assert_eq!(entity1, entity2);
}

#[test]
fn roundtrip_combinators() {
    let kinds = vec![
        EntityKind::ArithmeticCombinator {
            left: EntitySignal::constant(10),
            right: EntitySignal::constant(-200),
            output: EntitySignal::signal("signal-A"),
            operator: fbo::ArithmeticOperation::Modulo,
        },
        EntityKind::ArithmeticCombinator {
            left: EntitySignal::signal("car"),
            right: EntitySignal::constant(28),
            output: EntitySignal::signal("signal-A"),
            operator: fbo::ArithmeticOperation::Subtract,
        },
        EntityKind::ArithmeticCombinator {
            left: EntitySignal::signal("locomotive"),
            right: EntitySignal::constant(-421589),
            output: EntitySignal::signal("oil"),
            operator: fbo::ArithmeticOperation::LeftShift,
        },
        EntityKind::DeciderCombinator {
            left: EntitySignal::constant(12),
            right: EntitySignal::constant(-12),
            output: EntitySignal::signal("signal-Q"),
            operator: fbo::DeciderComparator::GreaterThanOrEqual,
            copy_count_from_input: false,
        },
        EntityKind::DeciderCombinator {
            left: EntitySignal::signal("signal-A"),
            right: EntitySignal::constant(-1002),
            output: EntitySignal::signal("signal-G"),
            operator: fbo::DeciderComparator::LessThan,
            copy_count_from_input: false,
        },
        EntityKind::DeciderCombinator {
            left: EntitySignal::constant(-19310212),
            right: EntitySignal::constant(-1312),
            output: EntitySignal::signal("signal-Q"),
            operator: fbo::DeciderComparator::GreaterThanOrEqual,
            copy_count_from_input: true,
        },
        EntityKind::ConstantCombinator {
            signals: {
                let mut s = BTreeMap::new();
                s.insert(
                    OneBasedIndex::new(1).unwrap(),
                    ConstantSignal::new("signal-A", 100),
                );
                s.insert(
                    OneBasedIndex::new(2).unwrap(),
                    ConstantSignal::new("signal-B", 123),
                );
                s.insert(
                    OneBasedIndex::new(3).unwrap(),
                    ConstantSignal::new("signal-C", 4321),
                );
                s
            },
        },
        EntityKind::ConstantCombinator {
            signals: {
                let mut s = BTreeMap::new();
                s.insert(
                    OneBasedIndex::new(1).unwrap(),
                    ConstantSignal::new("signal-A", 100),
                );
                s.insert(
                    OneBasedIndex::new(3).unwrap(),
                    ConstantSignal::new("signal-B", -123),
                );
                s.insert(
                    OneBasedIndex::new(5).unwrap(),
                    ConstantSignal::new("signal-C", -4321),
                );
                s
            },
        },
    ];

    kinds.into_iter().for_each(|kind| {
        roundtrip(Entity {
            entity_number: EntityNumber::try_from(123).unwrap(),
            name: "hello".to_string(),
            x: -41.4,
            y: 50000000.0,
            direction: fbo::Direction::East,
            connections: vec![
                (
                    OneBasedIndex::try_from(1).unwrap(),
                    EntityNumber::try_from(60).unwrap(),
                    1,
                    WireColor::Green,
                ),
                (
                    OneBasedIndex::try_from(1).unwrap(),
                    EntityNumber::try_from(12).unwrap(),
                    0,
                    WireColor::Green,
                ),
                (
                    OneBasedIndex::try_from(1).unwrap(),
                    EntityNumber::try_from(96).unwrap(),
                    0,
                    WireColor::Red,
                ),
                (
                    OneBasedIndex::try_from(2).unwrap(),
                    EntityNumber::try_from(23).unwrap(),
                    1,
                    WireColor::Red,
                ),
            ],
            kind,
        });
    });
}
