# dedekind

Exercises in strongly typed linear algebra on the JVM.

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/vincentk/dedekind/maven.yml?branch=main&style=flat-square)
[![license](https://img.shields.io/github/license/vincentk/dedekind.svg?style=flat-square)](LICENSE)

Roughly speaking an attempt to reproduce some results of [The simple essence of automatic differentiation](https://arxiv.org/abs/1804.00746#) in a modern core java (as opposed to haskell).

This is a multi-module maven project with the following layout:

* [dedekind-ontology](https://github.com/vincentk/dedekind/tree/main/dedekind-ontology) an attempt to express concepts from abstract algebra in the java type system as directly as possible.

* [dedekind-matrices](https://github.com/vincentk/dedekind/tree/main/dedekind-matrices) building on the former, provide some basic implementations of vectors, matrices and operations thereupon.

For build instructions, please refer to the [build pipeline](https://github.com/vincentk/dedekind/blob/main/.github/workflows/maven.yml).

## Known Limitations

Many, for the time being.

## Preliminary Results

* A fairly direct implementation of concepts from [set theory](https://github.com/vincentk/dedekind/blob/main/dedekind-ontology/src/main/java/com/github/vincentk/dedekind/sets/) (`Set`, `Cardinality`, ...) and [abstract algebra](https://github.com/vincentk/dedekind/tree/main/dedekind-ontology/src/main/java/com/github/vincentk/dedekind/algebra) (`Monoid`, `Module`, `Group`, `Field`, `Ring`, ...) as core java `interface` types. \
To facilitate later specialization of `interface` implementations, [higher-kinded types](https://www.baeldung.com/scala/higher-kinded-types) are emulated through recursive generics and `default` methods, e.g. about the following pattern:
```java
interface A<M extends A<M>> {
   default M plus(M that) {
     return this + that;
   }
}
```

* Sample implementations of scalar [number](https://github.com/vincentk/dedekind/tree/main/dedekind-ontology/src/main/java/com/github/vincentk/dedekind/numbers) systems for common types such as primitive types. \
`SemiRing`: `int` $\rightarrow \mathbb{N}$, \
`Ring`: `boolean` $\rightarrow \mathbb{B}$, `int` $\rightarrow \mathbb{Z}$, \
`Field`: `(int, int)` $\rightarrow \mathbb{Q}$, `double` $\rightarrow \mathbb{R}$, `(double, double)` $\rightarrow \mathbb{C}$ \
as well as some more advanced types such as [dual numbers](https://en.wikipedia.org/wiki/Dual_number).

* Some limited support for forward-mode automatic differentiation (via dual numbers).

* Vectorized operations in finite dimensions (tuples, e.g. $\mathcal C^n$) and infinite dimension (functions, e.g. $\mathcal C \rightarrow \mathcal C$) of either continuous values ($\mathcal R^n$, $\mathcal C^n$, ..., `Vector<F extends Field<F>>`) or discrete values ($\mathcal B^n$, $\mathcal N^n$, ..., `Module<R extends Ring<R>>`).

* Some limited support for type-checked bracket-type notation, e.g. inner $\braket{0|0}$ or outer $\ket{x}\bra{y}$ product spaces.

* Some limited support for lazy evaluation and "infinite" as well as sparse vectors and matrices. In particular, specific operations such as a transpose or outer (tensor) product may offer "infinite" speedup vis-a-vis common libraries as they may execute in $\mathcal{O}(0)$ as opposed to e.g. $\mathcal{O}(N)$.

* Similarly (again due to lazy evaluation), some symbolic manipulation is supported, e.g. $(A * B)^t = B^t * A^t$ or
$(A + B) * C = A * C + B * C$ can be evaluated symbolically and composed in $\mathcal{O}(0)$.

* Some support exists for typical operations such as concatenation and slicing.


### Implementation notes:

Notable challenges with the java type system which need to be overcome as compared to e.g. haskell or scala:

* Type erasure vs. polymorphism preventing an interface to be implemented multiple times with different arguments. I.e. while the default implementation for polymorphism in java is dynamic dispatch, a generic interface (`Foo<A>`)  declaring a method `foo(A)` can not be implemented twice with different parameters `Foo<X>` and `Foo<Y>`.
