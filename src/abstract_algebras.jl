"""
    AbstractAlgebra{A}

A wiring diagram algebra a is a higher-order function that
transforms each wiring diagram d into a multi-function

    a(d): X₁, ..., Xₙ → Y.

"""
abstract type AbstractAlgebra{A} end

"""
    AbstractAlgebra{A}

Given a wiring diagram algebra a, a wiring diagram d, and
a sequence

    (x₁, x₂, ..., xₙ)

of arguments, compute y:

    y = a(d)(x₁, x₂, ..., xₙ).

"""
apply(algebra::AbstractAlgebra, diagram::AbstractWiringDiagram, arguments)
