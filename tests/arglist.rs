#![cfg(test)]

#[macro_use]
mod macros;

test!(
    length_of_empty_arglist,
    "@mixin foo($a...) {\n    color: length($list: $a);\n}\na {\n    @include foo;\n}\n",
    "a {\n  color: 0;\n}\n"
);
test!(
    length_of_arglist_in_mixin,
    "@mixin foo($a...) {\n    color: length($list: $a);\n}\na {\n    @include foo(a, 2, c);\n}\n",
    "a {\n  color: 3;\n}\n"
);
test!(
    arglist_in_at_each,
    "@function sum($numbers...) {
        $sum: 0;
    
        @each $number in $numbers {
            $sum: $sum + $number;
        }
    
        @return $sum;
    }
    
    a {
        width: sum(50px, 30px, 100px);
    }",
    "a {\n  width: 180px;\n}\n"
);
