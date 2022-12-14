// #[derive(Debug)]
// pub(crate) struct ParsedNumber {
//     /// The full number excluding the decimal
//     ///
//     /// E.g. for `1.23`, this would be `"123"`
//     pub num: String,

//     /// The length of the decimal
//     ///
//     /// E.g. for `1.23`, this would be `2`
//     pub dec_len: usize,

//     /// The number following e in a scientific notated number
//     ///
//     /// E.g. for `1e23`, this would be `"23"`,
//     /// for `1`, this would be an empty string
//     // TODO: maybe we just return a bigint?
//     pub times_ten: String,

//     /// Whether or not `times_ten` is negative
//     ///
//     /// E.g. for `1e-23` this would be `true`,
//     /// for `1e23` this would be `false`
//     pub times_ten_is_postive: bool,
// }

// impl ParsedNumber {
//     pub const fn new(
//         num: String,
//         dec_len: usize,
//         times_ten: String,
//         times_ten_is_postive: bool,
//     ) -> Self {
//         Self {
//             num,
//             dec_len,
//             times_ten,
//             times_ten_is_postive,
//         }
//     }
// }
