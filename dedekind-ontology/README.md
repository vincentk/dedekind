# dedekind-ontology

A collection of type declarations attempting to

1. Embed concepts from abstract algebra in the java type system.
2. Relate the resulting java types to java primitive values.

The goal is to add some support to type-checked statements such as:

* `double` values form a `Field`.
* A `double[]` is an element in a particular vector space.
* N &sub; Z &sub; Q &sub; R &sub; C.
* `String` is a `Monoid` (via concatenation and the empty string).