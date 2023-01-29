# dedekind-ontology

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