/// #### Description
/// Asserts that `x` and `y` are equal by the given equivalence relation `pred`.
#[macro_export]
macro_rules! assert_eq_by {
    ($pred: expr, $x: expr, $y: expr) => {
        let pred_name: String = stringify!($pred).replace(" ", "");
        assert!(
            $pred($x, $y),
            "cannot assert `left == right by {}`\n  left: {:?}\n right: {:?}",
            pred_name,
            $x,
            $y
        );
    };
}
pub use assert_eq_by;

/// Common equivalence relations.
pub mod eqs {
    /// Equivalence relations for [Vec].
    pub mod vec {
        /// An equivalence relation for [Vec] that relates any two vectors
        /// containing the same elements (disregarding order).
        pub fn no_order<A: PartialEq>(xs: &[A], ys: &[A]) -> bool {
            xs.len() == ys.len() && ys.iter().all(|y| xs.contains(y))
        }
    }
}
