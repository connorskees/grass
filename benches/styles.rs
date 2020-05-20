use criterion::{black_box, criterion_group, criterion_main, Criterion};
use grass::StyleSheet;

pub fn many_foo(c: &mut Criterion) {
    c.bench_function("many_foo", |b| {
        b.iter(|| StyleSheet::new(black_box(include_str!("many_foo.scss").to_string())))
    });
}

criterion_group!(benches, many_foo);
criterion_main!(benches);
