use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;
use tr::tr;

pub fn short_literal(c: &mut Criterion) {
    c.bench_function("short_literal", |b| {
        b.iter(|| {
            tr!("Hello");
        })
    });
}

pub fn long_literal(c: &mut Criterion) {
    c.bench_function("long_literal", |b| b.iter(|| {
        tr!("Hello, world! This is a longer sentence but without argument markers. That is all for now, thank you for reading.");
    }));
}

pub fn short_argument(c: &mut Criterion) {
    c.bench_function("short_argument", |b| {
        b.iter(|| {
            tr!("Hello {}!", black_box("world"));
        })
    });
}

pub fn long_argument(c: &mut Criterion) {
    c.bench_function("long_argument", |b| {
        b.iter(|| {
            tr!(
                "Hello {} and {} and {} and {} and {} and {} and {} and finally {}!",
                black_box("Mercury"),
                black_box("Venus"),
                black_box("Earth"),
                black_box("Mars"),
                black_box("Jupiter"),
                black_box("Saturn"),
                black_box("Uranus"),
                black_box("Neptune"),
            );
        })
    });
}

criterion_group!(
    benches,
    short_literal,
    long_literal,
    short_argument,
    long_argument
);
criterion_main!(benches);
