use criterion::{black_box, criterion_group, criterion_main, Criterion};
use grass::StyleSheet;

pub fn many_variable_redeclarations(c: &mut Criterion) {
    c.bench_function("many_variable_redeclarations", |b| {
        b.iter(|| {
            StyleSheet::new(black_box(
                include_str!("many_variable_redeclarations.scss").to_string(),
            ))
        })
    });
}

criterion_group!(benches, many_variable_redeclarations);
criterion_main!(benches);
