use criterion::{black_box, criterion_group, criterion_main, Criterion};
use grass::StyleSheet;

pub fn many_floats(c: &mut Criterion) {
    c.bench_function("many_floats", |b| {
        b.iter(|| StyleSheet::new(black_box(include_str!("many_floats.scss").to_string())))
    });
}

pub fn many_integers(c: &mut Criterion) {
    c.bench_function("many_integers", |b| {
        b.iter(|| StyleSheet::new(black_box(include_str!("many_integers.scss").to_string())))
    });
}

pub fn many_small_integers(c: &mut Criterion) {
    c.bench_function("many_small_integers", |b| {
        b.iter(|| {
            StyleSheet::new(black_box(
                include_str!("many_small_integers.scss").to_string(),
            ))
        })
    });
}

criterion_group!(benches, many_floats, many_integers, many_small_integers);
criterion_main!(benches);
