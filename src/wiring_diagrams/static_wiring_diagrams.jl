struct StaticWiringDiagram{L, Prt, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl} <: AbstractWiringDiagram{Int, L} end

function StaticWiringDiagram(diagram::AbstractWiringDiagram{I, L}) where {I <: Integer, L}
    B = nb(diagram)
    W = nw(diagram)
    P = np(diagram)
    Q = nop(diagram)

    Prt = ntuple(B) do b
        return Tuple(ports(diagram, b))
    end

    Wre = ntuple(P) do p
        return wire(diagram, p)
    end

    OutWre = ntuple(Q) do q
        return outwire(diagram, q)
    end
    
    Lbl = ntuple(W) do w
        return label(diagram, w)
    end

    PrtLbl = ntuple(P) do p
        return portlabel(diagram, p)
    end

    OutPrtLbl = ntuple(Q) do q
        return outportlabel(diagram, q)
    end

    return StaticWiringDiagram{L, Prt, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}()
end

function StaticWiringDiagram(args...; kw...)
    return StaticWiringDiagram(WiringDiagram(args...; kw...))
end

function Base.show(io::IO, ::Type{<:StaticWiringDiagram{L}}) where {L}
    print(io, "StaticWiringDiagram{$L, …}")
end

# --------------------------------- #
# Abstract Wiring Diagram Interface #
# --------------------------------- #

function nb(diagram::StaticWiringDiagram{<:Any, Prt}) where {Prt}
    return length(Prt)
end

function nw(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, <:Any, Lbl}) where {Lbl}
    return length(Lbl)
end

function np(diagram::StaticWiringDiagram{<:Any, <:Any, Wre}) where {Wre}
    return length(Wre)
end

function nop(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, OutWre}) where {OutWre}
    return length(OutWre)
end

function boxes(diagram::StaticWiringDiagram)
    return ntuple(identity, nb(diagram))
end

function wires(diagram::StaticWiringDiagram)
    return ntuple(identity, nw(diagram))
end

function wirelabels(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, <:Any, Lbl}) where {Lbl}
    return Lbl
end

function ports(diagram::StaticWiringDiagram)
    return ntuple(identity, np(diagram))
end

function portwires(diagram::StaticWiringDiagram{<:Any, <:Any, Wre}) where {Wre}
    return Wre
end

function outports(diagram::StaticWiringDiagram)
    return ntuple(identity, nop(diagram))
end

function outportwires(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, OutWre}) where {OutWre}
    return OutWre
end

function outportlabels(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, <:Any, <:Any, <:Any, OutPrtLbl}) where {OutPrtLbl}
    return OutPrtLbl
end

function np(diagram::StaticWiringDiagram, b::Integer)
    return length(ports(diagram, b))
end

function ports(diagram::StaticWiringDiagram{<:Any, Prt}, b::Integer) where {Prt}
    return Prt[b]
end

function portwires(diagram::StaticWiringDiagram, b::Integer)
    return map(p -> wire(diagram, p), ports(diagram, b))
end

function portlabels(diagram::StaticWiringDiagram, b::Integer)
    return map(p -> portlabel(diagram, p), ports(diagram, b))
end

function label(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, <:Any, Lbl}, w::Integer) where {Lbl}
    return Lbl[w]
end

function wire(diagram::StaticWiringDiagram{<:Any, <:Any, Wre}, p::Integer) where {Wre}
    return Wre[p]
end

function portlabel(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, <:Any, <:Any, PrtLbl}, p::Integer) where {PrtLbl}
    return PrtLbl[p]
end

function outwire(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, OutWre}, q::Integer) where {OutWre}
    return OutWre[q]
end

function outportlabel(diagram::StaticWiringDiagram{<:Any, <:Any, <:Any, <:Any, <:Any, <:Any, OutPrtLbl}, q::Integer) where {OutPrtLbl}
    return OutPrtLbl[q]
end

