use criterion::{black_box, criterion_group, criterion_main, Criterion};
use grass::StyleSheet;

pub fn big_for(c: &mut Criterion) {
    c.bench_function("big_for", |b| {
        b.iter(|| StyleSheet::new(black_box(include_str!("big_for.scss").to_string())))
    });
}

criterion_group!(benches, big_for);
criterion_main!(benches);
