struct Operation{Alg <: AbstractAlgebra, Dgm <: Union{AbstractWiringDiagram, AbstractDendrogram}}
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
