"""
    AbstractAlgebra{A}

A wiring diagram algebra.
"""
abstract type AbstractAlgebra{A} end

"""
    AbstractAlgebra{A}

Apply a wiring diagram algebra to a wiring diagram,
evaluating the resulting function on a sequence of
arguments.
"""
apply(algebra::AbstractAlgebra, diagram::AbstractWiringDiagram, arguments)
