"""
    Operation{I, L, Alg, Dgm} <: AbstractOperation{I, L}

A multi-function

    f: X₁, X₂, ..., Xₙ → Y

of the form

    f(x₁, x₂, ..., xₙ) = a(d)(x₁, x₂, ..., xₙ)

for some wiring diagram algebra a and wiring diagram d.
"""
struct Operation{I, L, Alg <: AbstractAlgebra, Dgm <: Union{AbstractWiringDiagram{I, L}, AbstractDendrogram{I, L}}} <: AbstractOperation{I, L}
    algebra::Alg
    diagram::Dgm
end

function (algebra::AbstractAlgebra)(diagram::AbstractWiringDiagram)
    return Operation(algebra, diagram)
end

function (algebra::AbstractAlgebra)(dendrogram::AbstractDendrogram)
    return Operation(algebra, dendrogram)
end

function (operation::Operation)(arguments...)
    return apply(operation.algebra, operation.diagram, arguments)
end

# ---------------------------- #
# Abstract Operation Interface #
# ---------------------------- #

function arity(operation::Operation)
    return arity(operation.diagram)
end

function eachargument(operation::Operation)
    return eachargument(operation.diagram)
end

function domain(operation::Operation, b::Integer)
    return domain(operation.diagram, b)
end

function codomain(operation::Operation)
    return codomain(operation.diagram)
end

function compose(i::Integer, outer::Operation, inner::Operation)
    algebra = outer.algebra
    diagram = compose(i, outer.diagram, inner.diagram)
    return Operation(algebra, diagram)
end
