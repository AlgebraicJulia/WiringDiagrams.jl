"""
    Dendrogram{I, L, Xnp, Lbl, Wre, PrtLbl, Out, OutPrtLbl} <: AbstractDendrogram{I, L}
"""
struct Dendrogram{
        I,
        L,
        XPrt <: AbstractVector{I},
        XOutPrt <: AbstractVector{I},
        XChd <: AbstractVector{I},
        XOutChd <: AbstractVector{I},
        XWreBox <: AbstractVector{I},
        Pnt <: AbstractVector{I},
        OutPnt <: AbstractVector{I},
        Chd <: AbstractVector{I},
        OutChd <: AbstractVector{I},
        Wre <: AbstractVector{I},
        OutWre <: AbstractVector{I},
        Lbl <: AbstractVector{L},
        PrtLbl <: AbstractVector{L},
        OutPrtLbl <: AbstractVector{L},
    } <: AbstractDendrogram{I, L}

    """
    B is equal to the set

        {1, ..., nb} ⊆ ℕ.

    """
    nb::I

    """
    C is equal to the set

        {1, ..., nob} ⊆ ℕ.

    """
    nob::I

    """
    W is equal to the set

        {1, ..., nw} ⊆ ℕ.

    """
    nw::I

    """
    P is equal to the set

        {1, ..., np} ⊆ ℕ.

    """
    np::I

    """
    Q is equal to the set

        {1, ..., nop} ⊆ ℕ.

    """
    nop::I

    """
    Each box b ∈ B is incident to the ports

        {xprt[b], ..., xprt[b + 1] - 1} ⊆ P.

    """
    xprt::XPrt

    """
    Each outer box c ∈ C is incident to the outer ports

        {xoutprt[c], ..., xoutprt[c + 1] - 1} ⊆ Q.

    """
    xoutprt::XOutPrt

    """
    Each outer box c ∈ C is incident to the boxes

        {chd[xchd[c]], ..., chd[xchd[c + 1] - 1]} ⊆ B.

    """
    xchd::XChd

    """
    Each outer box c ∈ C is incident to the outer boxes

        {outchd[xoutchd[c]], ..., outchd[xoutchd[c + 1] - 1]} ⊆ C.        

    """
    xoutchd::XOutChd

    """
    Each outer box c ∈ C is incident to the wires

        {xwrebox[c], ..., xwrebox[c + 1] - 1} ⊆ W.

    """
    xwrebox::XWreBox

    """
    Each box b ∈ B has parent 

        pnt[b] ∈ C.

    """
    pnt::Pnt

    """
    Each outer box c ∈ C has parent

        outpnt[c] ∈ C

    """
    outpnt::OutPnt

    """
    Each outer box c ∈ C is incident to the boxes

        {chd[xchd[c]], ..., chd[xchd[c + 1] - 1]} ⊆ B.

    """
    chd::Chd

    """
    Each outer box c ∈ C is incident to the outer boxes

        {outchd[xoutchd[c]], ..., outchd[xoutchd[c + 1] - 1]} ⊆ C.        

    """
    outchd::OutChd

    """
    Each port p ∈ P has wire

        wre[p] ∈ W.    

    """
    wre::Wre

    """
    Each outer port q ∈ Q has wire

        outwre[q] ∈ W.

    """
    outwre::OutWre

    """
    Each wire w ∈ W has label

        lbl[w] ∈ L.

    """
    lbl::Lbl

    """
    Each port p ∈ P has label

        prtlbl[p] ∈ L.

    """
    prtlbl::PrtLbl

    """
    Each outer port q ∈ Q has label

        outprtlbl[q] ∈ L.

    """
    outprtlbl::OutPrtLbl


    function Dendrogram{I, L, XPrt, XOutPrt, XChd, XOutChd, XWreBox, Pnt, OutPnt, Chd, OutChd, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}(
            nb::Integer,
            nob::Integer,
            nw::Integer,
            np::Integer,
            nop::Integer,
            xprt::AbstractVector,
            xoutprt::AbstractVector,
            xchd::AbstractVector,
            xoutchd::AbstractVector,
            xwrebox::AbstractVector,
            pnt::AbstractVector,
            outpnt::AbstractVector,
            chd::AbstractVector,
            outchd::AbstractVector,
            wre::AbstractVector,
            outwre::AbstractVector,
            lbl::AbstractVector,
            prtlbl::AbstractVector,
            outprtlbl::AbstractVector,    
        ) where {
            I <: Integer,
            L,
            XPrt <: AbstractVector{I},
            XOutPrt <: AbstractVector{I},
            XChd <: AbstractVector{I},
            XOutChd <: AbstractVector{I},
            XWreBox <: AbstractVector{I},
            Pnt <: AbstractVector{I},
            OutPnt <: AbstractVector{I},
            Chd <: AbstractVector{I},
            OutChd <: AbstractVector{I},
            Wre <: AbstractVector{I},
            OutWre <: AbstractVector{I},
            Lbl <: AbstractVector{L},
            PrtLbl <: AbstractVector{L},
            OutPrtLbl <: AbstractVector{L},
        }

        @assert !isnegative(nb)
        @assert ispositive(nob)
        @assert !isnegative(nw)
        @assert !isnegative(np)
        @assert !isnegative(nop)
        @assert nb < length(xprt)
        @assert nob < length(xoutprt)
        @assert nob < length(xchd)
        @assert nob < length(xoutchd)
        @assert nob < length(xwrebox)
        @assert nb <= length(pnt)
        @assert nob <= length(outpnt)
        @assert nb <= length(chd)
        @assert nob <= length(outchd)
        @assert np <= length(wre)
        @assert nop <= length(outwre)
        @assert nw <= length(lbl)
        @assert np <= length(prtlbl)
        @assert nop <= length(outprtlbl)

        return new{I, L, XPrt, XOutPrt, XChd, XOutChd, XWreBox, Pnt, OutPnt, Chd, OutChd, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}(
            nb,
            nob,
            nw,
            np,
            nop,
            xprt,
            xoutprt,
            xchd,
            xoutchd,
            xwrebox,
            pnt,
            outpnt,
            chd,
            outchd,
            wre,
            outwre,
            lbl,
            prtlbl,
            outprtlbl,            
        )
    end
end

const DDendrogram{I, L} = Dendrogram{
    I,
    L,
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{L},
    Vector{L},
    Vector{L},
}

const FDendrogram{I, L} = Dendrogram{
    I,
    L,
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{L},
    FVector{L},
    FVector{L},
}

function (::Type{Dendrogram{I, L, XPrt, XOutPrt, XChd, XOutChd, XWreBox, Pnt, OutPnt, Chd, OutChd, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}})(
        nb::Integer,
        nob::Integer,
        nw::Integer,
        np::Integer,
        nop::Integer,
    ) where {
        I <: Integer,
        L,
        XPrt <: AbstractVector{I},
        XOutPrt <: AbstractVector{I},
        XChd <: AbstractVector{I},
        XOutChd <: AbstractVector{I},
        XWreBox <: AbstractVector{I},
        Pnt <: AbstractVector{I},
        OutPnt <: AbstractVector{I},
        Chd <: AbstractVector{I},
        OutChd <: AbstractVector{I},
        Wre <: AbstractVector{I},
        OutWre <: AbstractVector{I},
        Lbl <: AbstractVector{L},
        PrtLbl <: AbstractVector{L},
        OutPrtLbl <: AbstractVector{L},
    }

    xprt = XPrt(undef, nb + 1)
    xoutprt = XOutPrt(undef, nob + 1)
    xchd = XChd(undef, nob + 1)
    xoutchd = XOutChd(undef, nob + 1)
    xwrebox = XWreBox(undef, nob + 1)
    pnt = Pnt(undef, nb)
    outpnt = OutPnt(undef, nob)
    chd = Chd(undef, nb)
    outchd = OutChd(undef, nob)
    wre = Wre(undef, np)
    outwre = OutWre(undef, nop)
    lbl = Lbl(undef, nw)
    prtlbl = PrtLbl(undef, np)
    outprtlbl = OutPrtLbl(undef, nop)

    return Dendrogram{I, L, XPrt, XOutPrt, XChd, XOutChd, XWreBox, Pnt, OutPnt, Chd, OutChd, Wre, OutWre, Lbl, PrtLbl, OutPrtLbl}(
        nb,
        nob,
        nw,
        np,
        nop,
        xprt,
        xoutprt,
        xchd,
        xoutchd,
        xwrebox,
        pnt,
        outpnt,
        chd,
        outchd,
        wre,
        outwre,
        lbl,
        prtlbl,
        outprtlbl,            
    )
end

function Dendrogram{I, L}(nb::Integer, nob::Integer, nw::Integer, np::Integer, nop::Integer) where {I <: Integer, L}
    return FDendrogram{I, L}(nb, nob, nw, np, nop)
end

function Dendrogram(diagram::WiringDiagram{I}; kw...) where {I <: Integer}
    return Dendrogram(uniformweight, diagram, kw...)
end

function Dendrogram(f::Function, diagram::WiringDiagram; alg::PermutationOrAlgorithm=DEFAULT_ELIMINATION_ALGORITHM, snd::SupernodeType=DEFAULT_SUPERNODE_TYPE)
    return Dendrogram(f, diagram, alg, snd)
end

function Dendrogram(f::Function, diagram::WiringDiagram{I, L}, alg::PermutationOrAlgorithm, snd::SupernodeType) where {I <: Integer, L}
    B = nb(diagram)
    W = nw(diagram)
    P = np(diagram)
    Q = nop(diagram)

    wrebox = BipartiteGraph(diagram)
    boxwre = reverse(wrebox)
    wrewre = linegraph(boxwre, wrebox)

    weight = FVector{Float64}(undef, W)
    clique = FVector{I}(undef, W)
    invp = FVector{I}(undef, W)

    for w in wires(diagram)
        weight[w] = f(label(diagram, w))
    end

    # compute a tree (forest) decomposition of wrewre
    perm, tree = cliquetree(weight, wrewre, alg, snd)
    sep = separators(tree)
    res = residuals(tree)
    chd = tree.tree.tree.graph # lol

    for w in wires(diagram)
        invp[perm[w]] = w
    end

    # ensure that the wires incident to the outer box
    # are ordered last
    k = zero(I)

    for w in neighbors(wrebox, B + one(I))
        k += one(I); clique[k] = invp[w]
    end 

    permute!(perm, cliquetree!(tree, view(clique, oneto(k))))
    permute!(boxwre, perm, vertices(wrebox))

    for w in wires(diagram)
        invp[perm[w]] = w
    end

    # construct dendrogram
    C = nv(sep)
    W = ne(sep) + W
    Q = ne(sep) + Q
    dnd = Dendrogram{I, L}(B, C, W, P, Q)

    # define xprt
    for b in boxes(diagram)
        dnd.xprt[b] = first(ports(diagram, b))
    end

    dnd.xprt[B + one(I)] = P + one(I)    

    # define prtlbl
    for p in ports(diagram)
        dnd.prtlbl[p] = portlabel(diagram, p)
    end

    # define outprtlbl
    for p in arcs(sep)
        w = targets(sep)[p]
        dnd.outprtlbl[p] = label(diagram, perm[w])
    end

    for q in outports(diagram)
        dnd.outprtlbl[q + ne(sep)] = outportlabel(diagram, q)
    end

    # define
    #   - xoutprt
    #   - xoutchd
    #   - xwrebox
    #   - xchd
    #   - pnt
    #   - outpnt
    #   - chd    
    #   - outchd
    #   - wre
    #   - outwre
    #   - lbl
    wreidx = clique; pwstrt = pc = one(I)

    for b in boxes(diagram)
        dnd.pnt[b] = zero(I)
    end

    for c in vertices(sep)
        cc = parentindex(tree, c)

        if isnothing(cc)
            cc = C
        end
        
        dnd.xchd[c] = pc
        dnd.outpnt[c] = cc
        dnd.outchd[c] = targets(chd)[c]
        dnd.xoutprt[c] = pointers(sep)[c]
        dnd.xoutchd[c] = pointers(chd)[c]
        dnd.xwrebox[c] = pwstrt

        cres = neighbors(res, c)
        csep = neighbors(sep, c)

        pw = pwstrt + convert(I, length(cres)) 

        for w in csep
            wreidx[w] = pw; pw += one(I)
        end

        for ww in cres, b in neighbors(boxwre, ww)
            if b <= B && iszero(dnd.pnt[b])
                dnd.pnt[b] = c; dnd.chd[pc] = b; pc += one(I)

                for p in ports(diagram, b)
                    w = wire(diagram, p); w = invp[w]

                    if w in cres
                        w += pwstrt - first(cres)
                    else
                        w = wreidx[w]
                    end

                    dnd.wre[p] = w; dnd.lbl[w] = portlabel(diagram, p)
                end
            end
        end

        for cc in childindices(tree, c), q in incident(sep, cc)
            w = targets(sep)[q]

            if w in cres
                w += pwstrt - first(cres)
            else
                w = wreidx[w]
            end

            dnd.outwre[q] = w; dnd.lbl[w] = dnd.outprtlbl[q]
        end

        if c == C
            q = ne(sep)

            for w in outportwires(diagram)
                w = invp[w]
                q += one(I); dnd.outwre[q] = w + pwstrt - first(cres)
            end
        end

        pwstrt = pw
    end

    dnd.xchd[C + one(I)] = B + one(I)
    dnd.xoutprt[C + one(I)] = Q + one(I)
    dnd.xoutchd[C + one(I)] = C
    dnd.xwrebox[C + one(I)] = W + one(I)
    return dnd
end

# ----------------------------- #
# Abstract Dendrogram Interface #
# ----------------------------- #

function nb(dendrogram::Dendrogram)
    return dendrogram.nb
end

function nob(dendrogram::Dendrogram)
    return dendrogram.nob
end

function nw(dendrogram::Dendrogram)
    return dendrogram.nw
end 

function np(dendrogram::Dendrogram)
    return dendrogram.np
end

function nop(dendrogram::Dendrogram)
    return dendrogram.nop
end

function boxes(dendrogram::Dendrogram)
    return oneto(nb(dendrogram))
end

function outboxes(dendrogram::Dendrogram)
    return oneto(nob(dendrogram))
end

function wires(dendrogram::Dendrogram)
    return oneto(nw(dendrogram))
end

function wirelabels(dendrogram::Dendrogram)
    @inbounds l = view(dendrogram.lbl, wires(dendrogram))
    return l
end

function ports(dendrogram::Dendrogram)
    return oneto(np(dendrogram))
end

function outports(dendrogram::Dendrogram)
    return oneto(nop(dendrogram))
end

@propagate_inbounds function np(dendrogram::Dendrogram{I}, b::Integer) where {I <: Integer}
    @boundscheck checkbounds(boxes(dendrogram), b)
    @inbounds p = ports(dendrogram, b)
    return last(p) - first(p) + one(I)
end

@propagate_inbounds function ports(dendrogram::Dendrogram{I}, b::Integer) where {I <: Integer}
    @boundscheck checkbounds(boxes(dendrogram), b)
    @inbounds strt = dendrogram.xprt[b]
    @inbounds stop = dendrogram.xprt[b + one(I)] - one(I)
    return strt:stop
end

@propagate_inbounds function portwires(dendrogram::Dendrogram, b::Integer)
    @boundscheck checkbounds(boxes(dendrogram), b)
    @inbounds w = view(dendrogram.wre, ports(dendrogram, b))
    return w
end

@propagate_inbounds function portlabels(dendrogram::Dendrogram, b::Integer)
    @boundscheck checkbounds(boxes(dendrogram), b)
    @inbounds l = view(dendrogram.prtlbl, ports(dendrogram, b))
    return l 
end

@propagate_inbounds function parent(dendrogram::Dendrogram, b::Integer)
    @boundscheck checkbounds(boxes(dendrogram), b)
    @inbounds c = dendrogram.pnt[b]
    return c
end

@propagate_inbounds function nop(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds q = outports(dendrogram, c)
    return last(q) - first(q) + one(I)
end

@propagate_inbounds function outports(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds strt = dendrogram.xoutprt[c]
    @inbounds stop = dendrogram.xoutprt[c + one(I)] - one(I)
    return strt:stop
end

@propagate_inbounds function outportwires(dendrogram::Dendrogram, c::Integer)
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds w = view(dendrogram.outwre, outports(dendrogram, c))
    return w
end

@propagate_inbounds function outportlabels(dendrogram::Dendrogram, c::Integer)
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds l = view(dendrogram.outprtlbl, outports(dendrogram, c))
    return l
end

@propagate_inbounds function outparent(dendrogram::Dendrogram, c::Integer)
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds cc = dendrogram.outpnt[c]
    return cc
end

@propagate_inbounds function nb(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds strt = dendrogram.xchd[c]
    @inbounds stop = dendrogram.xchd[c + one(I)] - one(I)
    return stop - strt + one(I)
end

@propagate_inbounds function boxes(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds strt = dendrogram.xchd[c]
    @inbounds stop = dendrogram.xchd[c + one(I)] - one(I)
    @inbounds b = view(dendrogram.chd, strt:stop)
    return b
end

@propagate_inbounds function nob(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds strt = dendrogram.xoutchd[c]
    @inbounds stop = dendrogram.xoutchd[c + one(I)] - one(I)
    return stop - strt + one(I)
end

@propagate_inbounds function outboxes(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds strt = dendrogram.xoutchd[c]
    @inbounds stop = dendrogram.xoutchd[c + one(I)] - one(I)
    @inbounds cc = view(dendrogram.outchd, strt:stop)
    return cc
end

@propagate_inbounds function nw(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds w = wires(dendrogram, c)
    return last(w) - first(w) + one(I)
end

@propagate_inbounds function wires(dendrogram::Dendrogram{I}, c::Integer) where {I <: Integer}
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds strt = dendrogram.xwrebox[c]
    @inbounds stop = dendrogram.xwrebox[c + one(I)] - one(I)
    return strt:stop
end

@propagate_inbounds function wirelabels(dendrogram::Dendrogram, c::Integer)
    @boundscheck checkbounds(outboxes(dendrogram), c)
    @inbounds l = view(dendrogram.lbl, wires(dendrogram, c))
    return l
end

@propagate_inbounds function label(dendrogram::Dendrogram, w::Integer)
    @boundscheck checkbounds(wires(dendrogram), w)
    @inbounds l = dendrogram.lbl[w]
    return l
end

@propagate_inbounds function wire(dendrogram::Dendrogram, p::Integer)
    @boundscheck checkbounds(ports(dendrogram), p)
    @inbounds w = dendrogram.wre[p]
    return w
end

@propagate_inbounds function portlabel(dendrogram::Dendrogram, p::Integer)
    @boundscheck checkbounds(ports(dendrogram), p)
    @inbounds l = dendrogram.prtlbl[p]
    return l
end

@propagate_inbounds function outwire(dendrogram::Dendrogram, q::Integer)
    @boundscheck checkbounds(outports(dendrogram), q)
    @inbounds w = dendrogram.outwre[q]
    return w
end

@propagate_inbounds function outportlabel(dendrogram::Dendrogram, q::Integer)
    @boundscheck checkbounds(outports(dendrogram), q)
    @inbounds l = dendrogram.outprtlbl[q]
    return l
end
