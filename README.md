# dedekind

Exercises in strongly typed linear algebra on the JVM.

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/vincentk/dedekind/maven.yml?branch=main&style=flat-square)
[![license](https://img.shields.io/github/license/vincentk/dedekind.svg?style=flat-square)](LICENSE)

Roughly speaking an attempt to reproduce some results of [The simple essence of automatic differentiation](https://arxiv.org/abs/1804.00746#) in a more constrained type system (i.e. modern core java as opposed to haskell).

This is a multi-module maven project with the following layout:

* [dedekind-ontology](https://github.com/vincentk/dedekind/tree/main/dedekind-ontology) an attempt to express concepts from abstract algebra in the java type system as directly as possible.

* [dedekind-matrices](https://github.com/vincentk/dedekind/tree/main/dedekind-matrices) building on the former, provide some basic implementations of vectors, matrices and operations thereupon.

For build instructions, please refer to the [build pipeline](https://github.com/vincentk/dedekind/blob/main/.github/workflows/maven.yml).

## Preliminary Results

* Some limited support for less common operations such as boolean-valued or integer-valued vectors and matrices (modules over a ring as opposed to vector spaces over a field) in addition to the more commonly supported real-valued and complex-valued operations.

* Some limited support for forward-mode automatic differentiation (via dual numbers).

* Some limited support for lazy evaluation and "infinite" as well as sparse vectors and matrices. In particular, specific operations such as a transpose or outer (tensor) product may offer "infinite" speedup vis-a-vis common libraries as they may execute in $\mathcal{O}(0)$ as opposed to e.g. $\mathcal{O}(N)$.

* Similarly (again due to lazy evaluation), some symbolic manipulation is supported, e.g. 
```math
(A * B)^t = B^t * A^t$
```
or
```math
(A + B) * C = A * C + B * C
```
can be evaluated symbolically (in $\mathcal{O}(0)$).

* Some support exists for typical operations such as concatenation and slicing.

## Known Limitations

Many, for the time being.


### Implementation notes:

Notable challenges with the java type system which need to be overcome as compared to e.g. haskell or scala:

* Lack of [higher-kinded types](https://www.baeldung.com/scala/higher-kinded-types) (roughly speaking, reverse generics). I.e. it's possible to say `Class<A>`, but not `A<Class>` with `A` a parameter to be supplied at a later stage. As a workaround, these can sometimes be simulated by way of passing the type as a recursive generic parameter, giving rise to patters such as `interface F<A extends F<A>>`, with the concrete type being resolved at
a later stage about as follows: `class C implements F<C>`.
* Type erasure vs. polymorphism preventing an interface to be implemented multiple times with different arguments. I.e. while the default implementation for polymorphism in java is dynamic dispatch, a generic interface (`Foo<A>`)  declaring a method `foo(A)` can not be implemented twice with different parameters `Foo<X>` and `Foo<Y>`.
