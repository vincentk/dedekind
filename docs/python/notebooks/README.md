# Python Notebook Demos

Each notebook asserts what it shows, so it is also a test: `make integration-test`
executes every notebook here headlessly, and each must import `dedekind`.

1. `01_terms_on_the_ring_and_the_field.ipynb`: the canonical ring `u64` and
   field `𝔽ₚ`; the README exhibit `{x > 3} ∩ {x < 5} = {4}` as one normal form;
   the term surface checked when built; the successor's image wrapping; the
   ring's polynomial normal form.
2. `02_pst_sets_by_evaluation.ipynb`: the sets over the truth chains 𝔹 and K₃,
   with `Unknown` as a verdict and an L-valued set read through its α-cuts.
