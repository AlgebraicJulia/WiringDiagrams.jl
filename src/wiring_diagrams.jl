"""
    WiringDiagram{I, L, XPrt, Lbl, Wre, PrtLbl, Out, OutPrtLbl} <: AbstractWiringDiagram{I, L}
"""
struct WiringDiagram{
        I,
        L,
        XPrt <: AbstractVector{I},
        Wre <: AbstractVector{I},
        OutWre <: AbstractVector{I},
        Lbl <: AbstractVector{L},
        PrtLbl <: AbstractVector{L},
        OutPrtLbl <: AbstractVector{L},
    } <: AbstractWiringDiagram{I, L}

    """
    B is equal to the set

        B = {1, ..., nb} ⊆ ℕ.

    """
    nb::I

    """
    W is equal to the set

        W = {1, ..., nw} ⊆ ℕ.

    """
    nw::I

    """
    P is equal to the set

        P = {1, ..., np} ⊆ ℕ.

    """
    np::I

    """
    Q is equal to the set

        Q = {1, ..., nop} ⊆ ℕ.

    """
    nop::I

    """
    Each box b ∈ B is indicent to the ports

        {xprt[b], ..., xprt[b + 1] - 1} ⊆ P.

    """
    xprt::XPrt

    """
    Each port p ∈ P is incident to the wire

        wre[p] ∈ W.

    """
    wre::Wre

    """
    Each outer port p ∈ O is incident to the wire

        outwre[p] ∈ W.

    """
    outwre::OutWre

    """
    Each wire w ∈ W has label

        lbl[w] ∈ L.

    """
    lbl::Lbl

    """
    For all ports p ∈ P,

        prtlbl[p] = lbl[wre[p]].

    """
    prtlbl::PrtLbl

    """
    For all outer ports p ∈ O,

        outprtlbl[p] = lbl[outwre[p]].

    """
    outprtlbl::OutPrtLbl

    function WiringDiagram{I, L, XPrt, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}(
            nb::Integer,
            nw::Integer,
            np::Integer,
            nop::Integer,
            xprt::AbstractVector,
            wre::AbstractVector,
            outwre::AbstractVector,
            lbl::AbstractVector,
            prtlbl::AbstractVector,
            outprtlbl::AbstractVector,
        ) where {
            I <: Integer,
            L,
            XPrt <: AbstractVector{I},
            Wre <: AbstractVector{I},
            OutWre <: AbstractVector{I},
            Lbl <: AbstractVector{L},
            PrtLbl <: AbstractVector{L},
            OutPrtLbl <: AbstractVector{L},
        }

        @assert !isnegative(nb)
        @assert !isnegative(nw)
        @assert !isnegative(np)
        @assert !isnegative(nop)
        @assert nb < length(xprt)
        @assert np <= length(wre)
        @assert nop <= length(outwre)
        @assert nw <= length(lbl)
        @assert np <= length(prtlbl)
        @assert nop <= length(outprtlbl)

        return new{I, L, XPrt, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}(
            nb,
            nw,
            np,
            nop,
            xprt,
            wre,
            outwre,
            lbl,
            prtlbl,
            outprtlbl,
        )
    end
end

const DWiringDiagram{I, L} = WiringDiagram{
    I,
    L,
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{L},
    Vector{L},
    Vector{L},
}

const FWiringDiagram{I, L} = WiringDiagram{
    I,
    L,
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{L},
    FVector{L},
    FVector{L},
}

function WiringDiagram(
        nb::I,
        nw::I,
        np::I,
        nop::I,
        xprt::XPrt,
        wre::Wre,
        outwre::OutWre,
        lbl::Lbl,
        prtlbl::PrtLbl,
        outprtlbl::OutPrtLbl,
    ) where {
        I <: Integer,
        L,
        XPrt <: AbstractVector{I},
        Wre <: AbstractVector{I},
        OutWre <: AbstractVector{I},
        Lbl <: AbstractVector{L},
        PrtLbl <: AbstractVector{L},
        OutPrtLbl <: AbstractVector{L},
    }
    return WiringDiagram{I, L, XPrt, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}(
        nb,
        nw,
        np,
        nop,
        xprt,
        wre,
        outwre,
        lbl,
        prtlbl,
        outprtlbl,
    )
end

function (::Type{WiringDiagram{I, L, XPrt, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}})(nb::Integer, nw::Integer, np::Integer, nop::Integer) where {
        I <: Integer,
        L,
        XPrt <: AbstractVector{I},
        Wre <: AbstractVector{I},
        OutWre <: AbstractVector{I},
        Lbl <: AbstractVector{L},
        PrtLbl <: AbstractVector{L},
        OutPrtLbl <: AbstractVector{L},
    }

    xprt = XPrt(undef, nb + 1)
    wre = Wre(undef, np)
    outwre = OutWre(undef, nop)
    lbl = Lbl(undef, nw)
    prtlbl = PrtLbl(undef, np)
    outprtlbl = OutPrtLbl(undef, nop)

    return WiringDiagram{I, L, XPrt, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}(
        nb,
        nw,
        np,
        nop,
        xprt,
        wre,
        outwre,
        lbl,
        prtlbl,
        outprtlbl,
    )
end

"""
    WiringDiagram{I, L}(nb::Integer, nw::Integer, np::Integer, nop::Integer) where {I <: Integer, L}

Construct an uninitialized diagram with dimensions

    |B| = nb
    |W| = nw
    |P| = np
    |O| = nop.

"""
function WiringDiagram{I, L}(nb::Integer, nw::Integer, np::Integer, nop::Integer) where {I <: Integer, L}
    return FWiringDiagram{I, L}(nb, nw, np, nop)
end

function (::Type{Dgm})(diagram::WiringDiagram) where {Dgm <: WiringDiagram}
    return Dgm(
        diagram.nb,
        diagram.nw,
        diagram.np,
        diagram.nop,
        diagram.xprt,
        diagram.wre,
        diagram.outwre,
        diagram.lbl,
        diagram.prtlbl,
        diagram.outprtlbl,
    )

    return newdiagram
end

"""
    WiringDiagram(inputs::AbstractVector, output::AbstractVector[, label::AbstractDict])

Construct a wiring diagram. The vector `inputs` specifies the
function

    port(b, i) := inputs[b][i]

for all boxes b ∈ B and i ∈ {1, ..., |box⁻¹(b)|}. The
vector `output` specifies the function

    outwire(q) := output[q]

for all outer ports q ∈ Q, and the dictionary `label` specifies
the function

    label(w) := label[w]

for all wires w ∈ W. If `label` is omitted, then the constructed
wiring diagram will be unlabeled (i.e. all labels will be set to
`nothing`).
"""
function (::Type{Dgm})(inputs::AbstractVector, output::AbstractVector{S}, label::AbstractDict{S}) where {I <: Integer, Dgm <: WiringDiagram{I}, S}
    index = Dict{S, I}(); B = W = P = Q = zero(I)

    for input in inputs
        B += one(I)

        for s in input
            P += one(I)

            if !haskey(index, s)
                index[s] = W += one(I)
            end
        end
    end

    for s in output
        Q += one(I)

        if !haskey(index, s)
            index[s] = W += one(I)
        end
    end

    diagram = Dgm(B, W, P, Q); b = p = q = one(I)

    for input in inputs
        diagram.xprt[b] = p; b += one(I)

        for s in input
            diagram.wre[p] = index[s]; diagram.prtlbl[p] = label[s]; p += one(I)
        end
    end

    diagram.xprt[B + one(I)] = p

    for s in output
        diagram.outwre[q] = index[s]; diagram.outprtlbl[q] = label[s]; q += one(I)
    end

    for (s, w) in index
        diagram.lbl[w] = label[s]
    end

    return diagram
end

function WiringDiagram{I}(inputs::AbstractVector, output::AbstractVector, label::AbstractDict{<:Any, L}) where {I <: Integer, L}
    return WiringDiagram{I, L}(inputs, output, label)
end

function WiringDiagram(inputs::AbstractVector, output::AbstractVector, label::AbstractDict)
    return WiringDiagram{Int}(inputs, output, label)
end

function (::Type{Dgm})(inputs::AbstractVector, output::AbstractVector{S}) where {I <: Integer, Dgm <: WiringDiagram{I, Nothing}, S}
    index = Dict{S, I}(); B = W = P = Q = zero(I)

    for input in inputs
        B += one(I)

        for s in input
            P += one(I)

            if !haskey(index, s)
                index[s] = W += one(I)
            end
        end
    end

    for s in output
        Q += one(I)

        if !haskey(index, s)
            index[s] = W += one(I)
        end
    end

    diagram = Dgm(B, W, P, Q); b = p = q = one(I)

    for input in inputs
        diagram.xprt[b] = p; b += one(I)

        for s in input
            diagram.wre[p] = index[s]; p += one(I)
        end
    end

    diagram.xprt[B + one(I)] = p

    for s in output
        diagram.outwre[q] = index[s]; q += one(I)
    end

    return diagram
end

function WiringDiagram{I}(inputs::AbstractVector, output::AbstractVector) where {I <: Integer}
    return WiringDiagram{I, Nothing}(inputs, output)
end

function WiringDiagram(inputs::AbstractVector, output::AbstractVector)
    return WiringDiagram{Int}(inputs, output)
end

function WiringDiagram(workspace::Workspace{I, L}, dendrogram::AbstractDendrogram{I, L}, c::Integer) where {I <: Integer, L}
    @assert nob(dendrogram) > c >= one(I)

    B = P = zero(I)
    W = nw(dendrogram, c)
    Q = nop(dendrogram, c)

    offset = first(wires(dendrogram, c)) - one(I)

    for cc in outboxes(dendrogram, c)
        B += one(I)
        workspace.xprt[B] = P + one(I)

        for q in outports(dendrogram, cc)
            P += one(I)
            workspace.wre[P] = outwire(dendrogram, q) - offset
            workspace.prtlbl[P] = outportlabel(dendrogram, q)
        end
    end

    for b in boxes(dendrogram, c)
        B += one(I)
        workspace.xprt[B] = P + one(I)

        for p in ports(dendrogram, b)
            P += one(I)
            workspace.wre[P] = wire(dendrogram, p) - offset
            workspace.prtlbl[P] = portlabel(dendrogram, p)
        end
    end

    workspace.xprt[B + one(I)] = P + one(I)

    return WiringDiagram(
        B,
        W,
        P,
        Q,
        workspace.xprt,
        workspace.wre,
        W - Q + one(I):W,
        wirelabels(dendrogram, c),
        workspace.prtlbl,
        outportlabels(dendrogram, c),
    )
end

function WiringDiagram(workspace::Workspace{I, L}, dendrogram::AbstractDendrogram{I, L}) where {I <: Integer, L}
    B = P = Q = zero(I)
    C = nob(dendrogram)
    W = nw(dendrogram, C)

    offset = first(wires(dendrogram, C)) - one(I)

    for c in outboxes(dendrogram, C)
        B += one(I)
        workspace.xprt[B] = P + one(I)

        for q in outports(dendrogram, c)
            P += one(I)
            workspace.wre[P] = outwire(dendrogram, q) - offset
            workspace.prtlbl[P] = outportlabel(dendrogram, q)
        end
    end

    for b in boxes(dendrogram, C)
        B += one(I)
        workspace.xprt[B] = P + one(I)

        for p in ports(dendrogram, b)
            P += one(I)
            workspace.wre[P] = wire(dendrogram, p) - offset
            workspace.prtlbl[P] = portlabel(dendrogram, p)
        end
    end

    workspace.xprt[B + one(I)] = P + one(I)

    for w in outportwires(dendrogram, C)
        Q += one(I)
        workspace.outwre[Q] = w - offset
    end

    return WiringDiagram(
        B,
        W,
        P,
        Q,
        workspace.xprt,
        workspace.wre,
        workspace.outwre,
        wirelabels(dendrogram, C),
        workspace.prtlbl,
        outportlabels(dendrogram, C),
    )
end

# --------------------------------- #
# Abstract Wiring Diagram Interface #
# --------------------------------- #

function nb(diagram::WiringDiagram)
    return diagram.nb
end

function nw(diagram::WiringDiagram)
    return diagram.nw
end

function np(diagram::WiringDiagram)
    return diagram.np
end

function nop(diagram::WiringDiagram)
    return diagram.nop
end

function boxes(diagram::WiringDiagram)
    return oneto(nb(diagram))
end

function wires(diagram::WiringDiagram)
    return oneto(nw(diagram))
end

function wirelabels(diagram::WiringDiagram)
    @inbounds l = view(diagram.lbl, wires(diagram))
    return l
end

function ports(diagram::WiringDiagram)
    return oneto(np(diagram))
end

function portwires(diagram::WiringDiagram)
    @inbounds w = view(diagram.wre, ports(diagram))
    return w
end

function outports(diagram::WiringDiagram)
    return oneto(nop(diagram))
end

function outportwires(diagram::WiringDiagram)
    @inbounds w = view(diagram.outwre, outports(diagram))
    return w
end

function outportlabels(diagram::WiringDiagram)
    @inbounds l = view(diagram.outprtlbl, outports(diagram))
    return l
end

@propagate_inbounds function np(diagram::WiringDiagram{I}, b::Integer) where {I <: Integer}
    @boundscheck checkbounds(boxes(diagram), b)
    @inbounds p = ports(diagram, b)
    return last(p) - first(p) + one(I)
end

@propagate_inbounds function ports(diagram::WiringDiagram{I}, b::Integer) where {I <: Integer}
    @boundscheck checkbounds(boxes(diagram), b)
    @inbounds strt = diagram.xprt[b]
    @inbounds stop = diagram.xprt[b + one(I)] - one(I)
    return strt:stop
end

@propagate_inbounds function portwires(diagram::WiringDiagram, b::Integer)
    @boundscheck checkbounds(boxes(diagram), b)
    @inbounds w = view(diagram.wre, ports(diagram, b))
    return w
end

@propagate_inbounds function portlabels(diagram::WiringDiagram, b::Integer)
    @boundscheck checkbounds(boxes(diagram), b)
    @inbounds w = view(diagram.prtlbl, ports(diagram, b))
    return w
end

@propagate_inbounds function label(diagram::WiringDiagram, w::Integer)
    @boundscheck checkbounds(wires(diagram), w)
    @inbounds l = diagram.lbl[w]
    return l
end

@propagate_inbounds function wire(diagram::WiringDiagram, p::Integer)
    @boundscheck checkbounds(ports(diagram), p)
    @inbounds w = diagram.wre[p]
    return w
end

@propagate_inbounds function portlabel(diagram::WiringDiagram, p::Integer)
    @boundscheck checkbounds(ports(diagram), p)
    @inbounds l = diagram.prtlbl[p]
    return l
end

@propagate_inbounds function outwire(diagram::WiringDiagram, q::Integer)
    @boundscheck checkbounds(outports(diagram), q)
    @inbounds w = diagram.outwre[q]
    return w
end

@propagate_inbounds function outportlabel(diagram::WiringDiagram, q::Integer)
    @boundscheck checkbounds(outports(diagram), q)
    @inbounds l = diagram.outprtlbl[q]
    return l
end

