"""
    Operation{I, L, Alg, Dgm} <: AbstractOperation{I, L}

A function

```math
f: X_1 \\times \\dots \\times X_n \\to Y
```

of the form

```math
f(x_1, x_2, \\ldots, x_n) = a(d)(x_1, x_2, \\ldots, x_n)
```

for some wiring diagram algebra ``a`` and wiring diagram ``d``.
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
