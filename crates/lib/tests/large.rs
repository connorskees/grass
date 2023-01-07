//! Large tests that exercise many features at once

#[macro_use]
mod macros;

test!(
    /// https://github.com/kaj/rsass/issues/41
    issue_41,
    "@function str-replace($string, $search, $replace: \"\") {
      $index: str-index($string, $search);
    
      @if $index {
        @return str-slice($string, 1, $index - 1)+$replace+str-replace(str-slice($string, $index + str-length($search)), $search, $replace);
      }
    
      @return $index;
    }
    
    $x: str-replace(url(\"a#b#c\"), \"#\", \":\");
    
    a {
      color: $x;
    }",
    "a {\n  color: url(\"a:b:;\n}\n"
);

test!(
    base64_decode,
    r##"
@use "sass:math";
@use "sass:map";

@function char-at($s, $idx) {
  @return str-slice($s, $idx, $idx);
}

@function bitwise-and($a, $b) {
  $x: null;
  $y: null;

  @if $a > $b {
    $x: $a;
    $y: $b;
  } @else {
    $x: $b;
    $y: $a;
  }

  $limit: math.floor(math.log($x, 2));
  $result: 0;

  @for $n from 0 through $limit {
    $two-pow-n: math.pow(2, $n);
    $result: $result +
      $two-pow-n *
      (math.floor(math.div($x, $two-pow-n)) % 2) *
      (math.floor(math.div($y, $two-pow-n)) % 2);
  }

  @return $result;
}

@function bitwise-shift-left($n, $amt) {
  @return math.floor($n * math.pow(2, $amt));
}

@function bitwise-shift-right($n, $amt) {
  @return math.floor(math.div($n, math.pow(2, $amt)));
}

@function bitwise-or($a, $b) {
  $x: null;
  $y: null;

  @if $a > $b {
    $x: $a;
    $y: $b;
  } @else {
    $x: $b;
    $y: $a;
  }

  $limit: math.floor(math.log($x, 2));
  $result: 0;

  @for $n from 0 through $limit {
    $two-pow-n: math.pow(2, $n);
    $x-div-two-pow-n: math.floor(math.div($x, $two-pow-n)) % 2;
    $y-div-two-pow-n: math.floor(math.div($y, $two-pow-n)) % 2;

    $result: $result +
      $two-pow-n *
      (
        (
            ($x-div-two-pow-n + $y-div-two-pow-n) +
              $x-div-two-pow-n *
              $y-div-two-pow-n
          )
          %
          2
      );
  }

  @return $result;
}

@function letter-index-map() {
  $map: ();

  $chars: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

  @for $i from 1 through str-length($chars) {
    $char: char-at($chars, $i);

    $map: map.set($map, $char, $i - 1);
    $map: map.set($map, $i - 1, $char);
  }

  @return $map;
}

@function ascii-map() {
  $map: ();

  $chars-upper: "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  @for $i from 1 through str-length($chars-upper) {
    $char: char-at($chars-upper, $i);

    $map: map.set($map, $char, $i - 1 + 65);
    $map: map.set($map, $i - 1 + 65, $char);
  }

  $chars-lower: "abcdefghijklmnopqrstuvwxyz";

  @for $i from 1 through str-length($chars-lower) {
    $char: char-at($chars-lower, $i);

    $map: map.set($map, $char, $i - 1 + 97);
    $map: map.set($map, $i - 1 + 97, $char);
  }

  @return $map;
}

$letter-index-map: letter-index-map();
$ascii-map: ascii-map();

@function byte-at($str, $idx) {
  @return map-get($letter-index-map, char-at($str, $idx));
}

@function chunk-string($str) {
  $chunks: [];

  @for $i from 1 through str-length($str) {
    @if ($i - 1) % 4 == 0 {
      $chunk: byte-at($str, $i)
        byte-at($str, $i + 1)
        byte-at($str, $i + 2)
        byte-at($str, $i + 3);

      $chunks: append($chunks, $chunk);
    }
  }

  @return $chunks;
}

@function decode-base64($str) {
  $result: "";

  @if str-length($str) % 4 != 0 {
    @error "Invalid base64 string length";
  }

  $chunks: chunk-string($str);

  @each $chunk in $chunks {
    $start: bitwise-or(
      bitwise-shift-left(nth($chunk, 1), 18),
      bitwise-shift-left(nth($chunk, 2), 12)
    );

    @if bitwise-and(nth($chunk, 3), nth($chunk, 4)) == 64 {
      $result: "#{$result}#{map-get($ascii-map, bitwise-and(bitwise-shift-right($start, 16), 255))}";
    } @else if nth($chunk, 4) == 64 {
      $start: bitwise-or($start, bitwise-shift-left(nth($chunk, 3), 6));

      $result: "#{$result}#{map-get($ascii-map, bitwise-and(bitwise-shift-right($start, 16), 255))}";
      $result: "#{$result}#{map-get($ascii-map, bitwise-and(bitwise-shift-right($start, 8), 255))}";
    } @else {
      $val: bitwise-or(bitwise-shift-left(nth($chunk, 3), 6), nth($chunk, 4));
      $start: bitwise-or($start, $val);

      $result: "#{$result}#{map-get($ascii-map, bitwise-and(bitwise-shift-right($start, 16), 255))}";
      $result: "#{$result}#{map-get($ascii-map, bitwise-and(bitwise-shift-right($start, 8), 255))}";
      $result: "#{$result}#{map-get($ascii-map, bitwise-and(bitwise-shift-right($start, 0), 255))}";
    }
  }

  @return $result;
}

a {
  color: decode-base64("SGVsbG8=");
}
  "##,
    "a {\n  color: \"Hello\";\n}\n"
);

test!(
    two_sum,
    r#"
@use "sass:map";

@function two-sum($nums, $target) {
  $map: ();

  @for $i from 1 through length($nums) {
    $num: nth($nums, $i);

    $diff: $target - $num;

    @if map-get($map, $diff) {
      @return map-get($map, $diff) - 1 $i - 1;
    } @else {
      $map: map.set($map, $num, $i);
    }
  }

  @return null;
}

a {
  color: two-sum(2 7 11 15, 9);
  color: two-sum(3 2 4, 6);
  color: two-sum(3 3, 6);
  color: two-sum(3 2, 6);
}"#,
    "a {\n  color: 0 1;\n  color: 1 2;\n  color: 0 1;\n}\n"
);
