# dedekind-ontology

An attempt to express concepts from abstract algebra in the java type system as directly as possible.
The hope is to imitate terminology as one would find it in e.g.
1. U. Stammbach: [Lineare Algebra](https://people.math.ethz.ch/~stammb/linalg.html)
2. M. Spivak: Calculus On Manifolds


# Preliminary Results

* A fairly direct implementation of concepts from [set theory](https://github.com/vincentk/dedekind/blob/main/dedekind-ontology/src/main/java/com/github/vincentk/dedekind/sets/) (`Set`, `Cardinality`, ...) and [abstract algebra](https://github.com/vincentk/dedekind/tree/main/dedekind-ontology/src/main/java/com/github/vincentk/dedekind/algebra) as core java `interface` types.
  1. Operations on one set ('scalars'): (`Monoid`, `Group`, `Ring`, `Field`, ...)
  2. Operations on two sets ('vectors'): (`Module`, `Vector`, `ProductSpace`, ...)

* Sample implementations of scalar [number](https://github.com/vincentk/dedekind/tree/main/dedekind-ontology/src/main/java/com/github/vincentk/dedekind/numbers) systems for common types such as primitive types. \
`SemiRing`: `int` $\rightarrow \mathbb{N}$, \
`Ring`: `boolean` $\rightarrow \mathbb{B}$, `int` $\rightarrow \mathbb{Z}$, \
`Field`: `(int, int)` $\rightarrow \mathbb{Q}$, `double` $\rightarrow \mathbb{R}$, `(double, double)` $\rightarrow \mathbb{C}$ \
as well as some more advanced types such as [dual numbers](https://en.wikipedia.org/wiki/Dual_number).


* Symbolic (lazy) operations in 
  1. finite dimensions (tuples, e.g. $\mathbb C^n$) 
  2. infinite dimension (functions, e.g. $\mathbb C \rightarrow \mathbb C$) \
  of
  * continuous values ($\mathbb R^n$, $\mathbb C^n$, ..., `Vector<F extends Field<F>>`) or
  * discrete values ($\mathbb B^n$, $\mathbb Z^n$, ..., `Module<R extends Ring<R>>`).


## Implementation Notes:

A collection of type declarations attempting to provide

1. A "purely functional" embedding of concepts from abstract algebra (`Field`, `Module`, `Monoid`, `Ring`) in the java type system (`interface`).
2. An identification of the resulting java types to java primitive values (`byte`, `double`, `int`).
3. Extension of the above to provide sample implementations for e.g. complex numbers, dual numbers, rational numbers.


A goal being to provide some support for type-checked statements such as:


* Type checks for common subset operations such as N &sub; Z &sub; Q &sub; R &sub; C.
[Test case](src/test/java/com/github/vincentk/dedekind/sets/FieldsTest.java)

* Two-way bindings exist to and from java primitives.
E.g. it is possible to convert from a `short` to a rational number, negate it and convert back to a `short`.

* While algebras are distinct from the corresponding set elements, mappings exist e.g. from the set of real numbers to the field over the real numbers.
[Test case](src/test/java/com/github/vincentk/dedekind/linear/primitives/RsTest.java)

* `double` values can be coerced to satisfy the `Field` axioms.

* A `double[]` is an element in a particular vector space.



* `String` is a `Monoid` (via concatenation and the empty string).
* A `Matrix` is an implementation of a linear map.
* A complex number is a vector of size 2 using the complex conjugate to define an inner product.
* A vector is a "row vector" and can be transposed to a "column vector".
* A row vector is a 1 x N matrix.
* The unit vectors form an orthonormal `Basis`.