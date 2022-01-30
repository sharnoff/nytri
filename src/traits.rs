//! Abstration traits and implementations over datatypes

/// An individual value in the array-like structure being matched over
///
/// For almost all use-cases, the only implementation you will care about is the one for `u8`.  In
/// *some* cases (e.g. matching on raw UTF-16 provided as `&[u16]`), you might want to use the one
/// for `u16`. A similar case arises if you're implementing regex matching over a custom datatype.
///
/// In practice though, most customization can be better served by implementations of the [`Text`]
/// trait, which is a little higher level.
pub trait Atom: Sized + Ord + Clone {}

/// Text-like list to be matched over
pub trait Text {
    /// The atomic item type contained in the text
    type Atom: Atom;

    /// The number of `Atom`s contained in the text, or `None` if that value is greater than
    /// `usize::MAX`
    ///
    /// The size may also be infinite.
    fn size(&self) -> Option<usize>;
}
