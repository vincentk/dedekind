# dedekind-ontology

A collection of type declarations attempting to

1. Embed concepts from abstract algebra in the java type system.
2. Relate the resulting java types to java primitive values.

The goal is to add some type-checking support to statements such as:

* `double` values form a `Field`.
* A `double[]` is an element in a particular vector space.
* N &sub; Z &sub; Q &sub; R &sub; C.
* `String` is a `Monoid` (via concatenation and the empty string).
* A `Matrix` is an implementation of a linear map.
* A complex number is a vector of size 2 using the complex conjugate to define an inner product.
* A vector is a "row vector" and can be transposed to a "column vector".
* A row vector is a 1 x N matrix.
* The unit vectors form an orthonormal `Basis`.