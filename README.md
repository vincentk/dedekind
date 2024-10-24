# dedekind

Exercises in strongly typed linear algebra on the JVM.

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/vincentk/dedekind/maven.yml?branch=main&style=flat-square)
[![license](https://img.shields.io/github/license/vincentk/dedekind.svg?style=flat-square)](LICENSE)

An attempt to model and reproduce some results from 
* [The simple essence of automatic differentiation](https://arxiv.org/abs/1804.00746#) 
* [Lineare Algebra](https://people.math.ethz.ch/~stammb/linalg.html)

in a modern core java.

To facilitate later specialization of `interface` implementations, [higher-kinded types](https://www.baeldung.com/scala/higher-kinded-types) are emulated through recursive generics and `default` methods, following about the following pattern:
```java
interface A<M extends A<M>> {
   default M plus(M that) {
     return this.binaryOp(that);
   }
}
```

This is a multi-module maven project with the following layout:

* [dedekind-ontology](https://github.com/vincentk/dedekind/tree/main/dedekind-ontology) an attempt to express concepts from abstract algebra in the java type system as directly as possible.

* [dedekind-matrices](https://github.com/vincentk/dedekind/tree/main/dedekind-matrices) building on the former, provide some basic implementations of vectors, matrices and operations thereupon.

For build instructions, please refer to the [build pipeline](https://github.com/vincentk/dedekind/blob/main/.github/workflows/maven.yml).

## Known Limitations

Many, for the time being.

## Preliminary Results

* Some limited support for type-checked bracket-type notation, e.g. inner $\braket{0|0}$ or outer $\ket{x}\bra{y}$ product spaces.

* Some limited support for lazy evaluation and "infinite" as well as sparse vectors and matrices. In particular, specific operations such as a transpose or outer (tensor) product may offer "infinite" speedup vis-a-vis common libraries as they may execute in $\mathcal{O}(0)$ as opposed to e.g. $\mathcal{O}(N)$.

* Similarly (again due to lazy evaluation), some symbolic manipulation is supported, e.g. $(A * B)^t = B^t * A^t$ or
$(A + B) * C = A * C + B * C$ can be evaluated symbolically and composed in $\mathcal{O}(0)$.

* Some support exists for typical operations such as concatenation and slicing, as they can be implemented efficiently via matrix addition and multiplication, the intuition being as follows:
```math
\begin{eqnarray} 
\begin{bmatrix}a\end{bmatrix}    &=& \begin{bmatrix}1 & 0\end{bmatrix} \begin{bmatrix}a\\x\end{bmatrix}\\
\begin{bmatrix}a & b\end{bmatrix}&=& \begin{bmatrix}a\end{bmatrix} \begin{bmatrix}1 & 0\end{bmatrix} + \begin{bmatrix}b\end{bmatrix} \begin{bmatrix}0 & 1\end{bmatrix}
\end{eqnarray}
```



### Implementation notes:

Notable challenges with the java type system which need to be overcome as compared to e.g. haskell or scala:

* Type erasure vs. polymorphism preventing an interface to be implemented multiple times with different arguments. I.e. while the default implementation for polymorphism in java is dynamic dispatch, a generic interface (`Foo<A>`)  declaring a method `foo(A)` can not be implemented twice with different parameters `Foo<X>` and `Foo<Y>`.
