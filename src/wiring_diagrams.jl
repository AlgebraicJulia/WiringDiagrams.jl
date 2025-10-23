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

################################
# Abstract Operation Interface #
################################

function compose(i::I, outer::AbstractWiringDiagram{I, L}, inner::AbstractWiringDiagram{I, L}) where {I <: Integer, L}
    # Let Do := outer
    #
    #          + ----------------- +
    #          |        Bo         |
    #          |        ↑ boxo     |
    #          |        Po         |
    #    Do := | labelo ↓ wireo    |
    #          |    L ← Wo         |
    #          |        ↑ outwireo |
    #          |        Qo         |
    #          + ----------------- +
    #
    Do = outer; Bo = nb(Do); Wo = nw(Do); Po = np(Do); Qo = nop(Do)

    # Di := inner is a diagram
    #
    #          + ----------------- +
    #          |        Bi         |
    #          |        ↑ boxi     |
    #          |        Pi         |
    #    Di := | labeli ↓ wirei    |
    #          |    L ← Wi         |
    #          |        ↑ outwirei |
    #          |        Qi         |
    #          + ----------------- +
    #
    # satisfying
    #
    #     Oi ≅ {po ∈ Po | boxo(po) = i}.
    #
    Di = inner; Bi = nb(Di); Wi = nw(Di); Pi = np(Di); Qi = nop(Di)

    # We wish to construct the composite diagram Dc := Do ∘i Di.
    #
    #          + ---------------- +
    #          |       Bc         |
    #          |       ↑ boxc     |
    #          |       Pc         |
    #    Dc := |  lblb ↓ wirec    |
    #          |   L ← Wc         |
    #          |       ↑ outwirec |
    #          |       Qc         |
    #          + ---------------- +
    #
    # We begin by computing a pushout (Wc, fo, fi).
    #
    #     + ------------------ +
    #     |           wireo    |
    #     |          Qi → Wo   |
    #     | outwirei ↓ ⌟  ↓ fo |
    #     |          Wi → Wc   |
    #     |            fi      |
    #     + ------------------ +
    #
    # We represent the legs fo and fi as a composite [fo, fi] := prj ; img of mappings
    #
    #     prj: Wo + Wi → Wo + Wi
    #     img: Wo + Wi → Wc.
    #
    # We also mainttain a mapping
    #
    #     inv: Wc → Wo + Wi
    #
    # satisfying prj = prj ; img ; inv. Initially, we set Wc = Wo + Wi
    # and prj = img = inv = id.
    Wc = Wo + Wi

    prj = UnionFind{I}(Wo + Wi)
    img = FVector{I}(undef, Wo + Wi)
    inv = FVector{I}(undef, Wo + Wi)

    prj.rank .= zero(I)
    prj.parent .= zero(I)
    img .= oneto(Wo + Wi)
    inv .= oneto(Wo + Wi)

    # For all ports qi ∈ Qi, let po ∈ Po be the port satisfying
    #
    #     qi ∼ po 
    #
    for (qi, po) in zip(outports(Di), ports(Do, i))
        # Let wo ∈ Wo be the wire 
        #
        #     wo := wireo(po)
        #
        wo = wire(Do, po)

        # Let wi ∈ Wi be the wire
        #
        #     wi := outwirei(qi)
        #
        wi = outwire(Di, qi)

        # Let ro ∈ Wo + Wi be the wire
        #
        #     ro := prj(wo)
        #
        ro = prj[wo]

        # Let ri ∈ Wo + Wi be the wire
        #
        #     ri := prj(wi)
        #
        ri = prj[Wo + wi]

        # If ro ≠ ri...
        if ro != ri
            # Let rx ∈ {ro, ri}.
            #
            # For all w ∈ Wo such that prj(w) = ro, set
            #
            #     prj(w) := rx.
            # 
            # For all w ∈ Wi such that prj(w) = ri, set
            #
            #     prj(w) := rx.
            #
            rx = union!(prj, ro, ri)

            # Let ry ∈ {ro, ri} satisfy rx != ry.
            if rx != ri
                ry = ri
            else
                ry = ro
            end

            # Let rz be a wire in
            # 
            #     im prj ⊆ Wo + Wi.
            #
            # Remove img(rz) from Wc.
            rz = inv[Wc]; Wc -= one(I)

            # If ry != rz...
            if ry != rz
                # Let wy ∈ Wc be the wire
                #
                #     wy := img(ry).
                #
                # Set img(rz) := ry and inv(ry) := rz.
                wy = img[rz] = img[ry]; inv[wy] = rz
            end
        end
    end

    # Next, we construct the mappings
    #
    #     fo: Wo → Wc
    #     fi: Wi → Wc
    #
    # by composing [fo, fi] := prj ; img.
    f = prj.rank

    for w in oneto(Wo + Wi)
        f[w] = img[prj[w]]
    end

    fo = f; fi = view(f, Wo + one(I):Wo + Wi)

    # Let Bo1, Bo2, Po1, Po2 be the sets
    #
    #     Bo1 := {bo ∈ Bo | bo < i} ⊆ Bo
    #     Bo2 := {bo ∈ Bo | bo > i} ⊆ Bo
    #
    #     Po1 := {po ∈ Po | boxo(po) < i } ⊆ Po
    #     Po2 := {po ∈ Po | boxo(po) > i } ⊆ Po
    #
    pos = ports(Do, i)
    Bo1 = i          - one(I); Bo2 = Bo - i
    Po1 = first(pos) - one(I); Po2 = Po - last(pos)

    # We construct Dc with dimensions
    #
    #     Bc := Bo1 + Bi + Bo2
    #     Pc := Po1 + Pi + Po2
    #     Oc := Oo
    #
    # along with the previously computed Wc.
    Bc = Bo1 + Bi + Bo2
    Pc = Po1 + Pi + Po2
    Qc = Qo
    Dc = WiringDiagram{I, L}(Bc, Wc, Pc, Qc)

    # The mapping boxc: Pc → Bc is given by
    #
    #     boxc := [boxo | Po1, boxi, boxo | Po2]
    #
    # where
    #
    #     boxo | Po1: Po1 → Bo1
    #     boxo | Po2: Po2 → Bo2
    #
    # are the restrictions of boxo to the sets Po1 and Po2.
    pc = one(I)

    # For all boxes bo ∈ Bo1
    for bo in oneto(Bo1)
        # For all ports po ∈ Po such that  
        #
        #     boxo(po) = bo,
        #
        # set boxc(po) := bo.
        Dc.xprt[bo] = pc; pc += np(Do, bo)
    end

    # for all boxes bi ∈ Bi...
    for bi in boxes(Di)
        # For all ports pi ∈ Pi such that  
        #
        #     boxi(pi) = bi,
        #
        # set boxc(pi) := bi.
        Dc.xprt[Bo1 + bi] = pc; pc += np(Di, bi)
    end

    # For all boxes bo ∈ Bo2...
    for bo in Bo - Bo2 + one(I):Bo
        # For all ports po ∈ Po such that  
        #
        #     boxo(po) = bo,
        #
        # set boxc(po) := bo.
        Dc.xprt[Bi + bo - one(I)] = pc; pc += np(Do, bo)
    end

    Dc.xprt[Bc + one(I)] = pc 

    # The mapping labelc: Wc → L is given by
    #   
    #     fo ; labelc := labelo
    #     fi ; labelc := labeli
    #  
    for wo in wires(Do)
        # Let wc ∈ Wc be the wire
        #
        #     wc := fo(wo)
        #
        # and set labelc(wc) := labelo(wo).
        wc = fo[wo]; Dc.lbl[wc] = label(Do, wo)
    end

    for wi in wires(Di)
        # Let wc ∈ Wc be the wire
        #
        #     wi := fi(wi)
        #
        # and set labelc(wc) := labeli(wi).
        wc = fi[wi]; Dc.lbl[wc] = label(Di, wi)
    end

    # The mapping wirec: Pc → Wc is given by
    #
    #     wirec := [wireo | Po1 ; fo, wirei ; fi, wireo | Po2 ; fo].
    #
    # where
    #
    #     wireo | Po1: Po1 → Wo
    #     wireo | Po2: Po2 → Wo
    #
    # are the restrictions of wireo to the sets Po1 and Po2.

    # For all ports po ∈ Po1...
    for po in oneto(Po1)
        # Let wo ∈ Wo be the wire
        #
        #    wo := wireo(po)
        #
        # and set wirec(po) := fo(wo).
        wo = wire(Do, po); Dc.wre[po] = fo[wo]; Dc.prtlbl[po] = portlabel(Do, po)
    end

    # For all ports pi ∈ Pi...
    for pi in ports(Di)
        # Let wi ∈ Wi be the wire
        #
        #    wi := wirei(pi)
        #
        # and set wirec(pi) := fi(wi).
        wi = wire(Di, pi); Dc.wre[Po1 + pi] = fi[wi]; Dc.prtlbl[Po1 + pi] = portlabel(Di, pi)
    end

    # For all ports po ∈ Po2...
    for po in Po - Po2 + one(I):Po
        # Let wo ∈ Wo be the wire
        #
        #    wo := wireo(po)
        #
        # and set wirec(po) := fo(wo).
        wo = wire(Do, po); Dc.wre[Pi - Qi + po] = fo[wo]; Dc.prtlbl[Pi - Qi + po] = portlabel(Do, po)
    end

    # The mapping outc: Qc → Wc is given by
    #
    #     outwirec := outwireo ; fo.
    #
    # For all ports qo ∈ Qo...
    for qo in outports(Do)
        # Let wo ∈ Wo be the wire
        #
        #     wo := outwireo(qo)
        #
        wo = outwire(Do, qo)

        # Let wc ∈ Wc be the wire
        #
        #     wc := fo(wo)
        #
        # and set outwirec(pd) := fo(wd).
        Dc.outwre[qo] = fo[wo]; Dc.outprtlbl[qo] = outportlabel(Do, qo)
    end

    return Dc
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
