use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn many_hsla(c: &mut Criterion) {
    c.bench_function("many_hsla", |b| {
        b.iter(|| {
            grass::from_string(
                black_box(include_str!("many_hsla.scss").to_string()),
                &Default::default(),
            )
        })
    });
}

pub fn many_named_colors(c: &mut Criterion) {
    c.bench_function("many_named_colors", |b| {
        b.iter(|| {
            grass::from_string(
                black_box(include_str!("many_named_colors.scss").to_string()),
                &Default::default(),
            )
        })
    });
}

criterion_group!(benches, many_hsla, many_named_colors,);
criterion_main!(benches);
