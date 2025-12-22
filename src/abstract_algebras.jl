"""
    AbstractAlgebra{A}

A wiring diagram algebra ``a`` is a higher-order function that
transforms each wiring diagram ``d`` into a function

```math
a(d): X_1 \\times \\dots \\times X_n \\to Y.
```
"""
abstract type AbstractAlgebra{A} end

"""
    apply(algebra::AbstractAlgebra, diagram::AbstractWiringDiagram, arguments)

Given a wiring diagram algebra ``a``, a wiring diagram ``d``, and
a sequence ``(x_1, x_2, \\ldots, x_n)`` of arguments, compute ``y``:

```math
y = a(d)(x_1, x_2, \\ldots, x_n).
```
"""
apply(algebra::AbstractAlgebra, diagram::AbstractWiringDiagram, arguments)
