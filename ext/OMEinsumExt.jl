module OMEinsumExt

using OMEinsum
using WiringDiagrams

function WiringDiagrams.apply(algebra::ArrayAlgebra, diagram::AbstractWiringDiagram, arguments)
    B = nb(diagram)

    tuple = ntuple(B) do b
        arg = arguments[b]
        return arg
    end

    return apply(algebra, diagram, tuple)
end

function WiringDiagrams.apply(algebra::ArrayAlgebra, diagram::AbstractWiringDiagram, arguments::Tuple)
    return einsum(eincode(diagram), arguments)
end

function eincode(diagram::AbstractWiringDiagram)
    ixs = map(boxes(diagram)) do b
        ix = collect(portwires(diagram, b))
        return ix
    end

    iy = collect(outportwires(diagram))

    return DynamicEinCode(ixs, iy)
end

function eincode(diagram::StaticWiringDiagram)
    ixs = map(boxes(diagram)) do b
        ix = portwires(diagram, b)
        return ix
    end

    iy = outportwires(diagram)

    return StaticEinCode{Int, ixs, iy}()
end

end
