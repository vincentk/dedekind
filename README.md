# dedekind

Exercises in strongly typed linear algebra on the JVM.

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/vincentk/dedekind/maven.yml?branch=main&style=flat-square)
[![license](https://img.shields.io/github/license/vincentk/dedekind.svg?style=flat-square)](LICENSE)

Roughly speaking an attempt to reproduce some results of http://conal.net/papers/essence-of-ad/ .


### Implementation notes:

Notable challenges with the java type system which need to be overcome as compared to e.g. haskell or scala:

* Lack of [higher-kinded types](https://www.baeldung.com/scala/higher-kinded-types) (roughly speaking, reverse generics). I.e. it's possible to say `Class<A>`, but not `A<Class>` with `A` a parameter to be supplied at a later stage. As a workaround, these can sometimes be simulated by way of passing the type as a recursive generic parameter, giving rise to patters such as `interface F<A extends F<A>>`, with the concrete type being resolved at
a later stage about as follows: `class C implements F<C>`.
* Type erasure vs. polymorphism preventing an interface to be implemented multiple times with different arguments. I.e. while the default implementation for polymorphism in java is dynamic dispatch, a generic interface (`Foo<A>`)  declaring a method `foo(A)` can not be implemented twice with different parameters `Foo<X>` and `Foo<Y>`.
