struct Workspace{
        I <: Integer,
        L,
        XPrt <: AbstractVector{I},
        Wre <: AbstractVector{I},
        OutWre <: AbstractVector{I},
        PrtLbl <: AbstractVector{L},
    }

    nb::I
    nw::I
    np::I
    nop::I
    xprt::XPrt
    wre::Wre
    outwre::OutWre
    prtlbl::PrtLbl

    function Workspace{I, L, XPrt, Wre, OutWre, PrtLbl}(
            nb::Integer,
            nw::Integer,
            np::Integer,
            nop::Integer,
            xprt::AbstractVector,
            wre::AbstractVector,
            outwre::AbstractVector,
            prtlbl::AbstractVector,
        ) where {
            I <: Integer,
            L,
            XPrt <: AbstractVector{I},
            Wre <: AbstractVector{I},
            OutWre <: AbstractVector{I},
            PrtLbl <: AbstractVector{I},
        }
    
        @assert !isnegative(nb)
        @assert !isnegative(nw)
        @assert !isnegative(np)
        @assert !isnegative(nop)
        @assert nb < length(xprt)
        @assert np <= length(wre)
        @assert nop <= length(outwre)
        @assert np <= length(prtlbl)

        return new{I, L, XPrt, Wre, OutWre, PrtLbl}(
            nb,
            nw,
            np,
            nop,
            xprt,
            wre,
            outwre,
            prtlbl,
        )
    end
end

const DWorkspace{I, L} = Workspace{
    I,
    L,
    Vector{I},
    Vector{I},
    Vector{I},
    Vector{L},
}

const FWorkspace{I, L} = Workspace{
    I,
    L,
    FVector{I},
    FVector{I},
    FVector{I},
    FVector{L},
}

function (::Type{Workspace{I, L, XPrt, Wre, OutWre, PrtLbl}})(nb::Integer, nw::Integer, np::Integer, nop::Integer) where {
        I <: Integer,
        L,
        XPrt <: AbstractVector{I},
        Wre <: AbstractVector{I},
        OutWre <: AbstractVector{I},
        PrtLbl <: AbstractVector{I},
    }

    xprt = XPrt(undef, nb + 1)
    wre = Wre(undef, np)
    outwre = OutWre(undef, nop)
    prtlbl = PrtLbl(undef, np)

    return Workspace{I, L, XPrt, Wre, OutWre, PrtLbl}(
        nb,
        nw,
        np,
        nop,
        xprt,
        wre,
        outwre,
        prtlbl,
    )
end 

function Workspace{I, L}(nb::Integer, nw::Integer, np::Integer, nop::Integer) where {I <: Integer, L}
    return FWorkspace{I, L}(nb, nw, np, nop)
end

function (::Type{Wrk})(dendrogram::AbstractDendrogram) where {I <: Integer, Wrk <: Workspace{I}}
    B = W = P = zero(I)

    for c in outboxes(dendrogram)
        Pc = zero(I) 

        for b in boxes(dendrogram, c)
            P += np(dendrogram, b)
        end

        for cc in outboxes(dendrogram, c)
            P += nop(dendrogram, cc)
        end

        Bc = nb(dendrogram, c) + nob(dendrogram, c)
        Wc = nw(dendrogram, c)

        B = max(B, Bc)
        W = max(W, Wc)
        P = max(P, Pc)
    end

    Q = nop(dendrogram, nob(dendrogram))
    return Wrk(B, W, P, Q)
end

function Workspace(dendrogram::AbstractDendrogram{I, L}) where {I <: Integer, L}
    return Workspace{I, L}(dendrogram)
end

function apply(algebra::AbstractAlgebra, dendrogram::AbstractDendrogram, arguments)
    workspace = Workspace(dendrogram)
    return apply!(workspace, algebra, dendrogram, arguments)
end

function apply!(workspace::Workspace, algebra::AbstractAlgebra{A}, dendrogram::AbstractDendrogram{I, L}, arguments) where {A, I <: Integer, L}
    B = nb(dendrogram)
    C = nob(dendrogram)

    start = zero(I)
    stack = FVector{A}(undef, B + C)

    for c in oneto(C - one(I))
        start = apply_node!(workspace, algebra, dendrogram, arguments, stack, start, c)
    end

    return apply_root!(workspace, algebra, dendrogram, arguments, stack, start)
end

function apply_node!(
        workspace::Workspace,
        algebra::AbstractAlgebra{A},
        dendrogram::AbstractDendrogram{I, L},
        arguments,
        stack::AbstractVector{A},
        strt::I,
        c::I,
    ) where {A, I <: Integer, L}

    stop = strt
    strt = strt - nob(dendrogram, c) + one(I)

    for b in boxes(dendrogram, c)
        stop += one(I); stack[stop] = arguments[b]
    end

    stack[strt] = apply(
        algebra,
        WiringDiagram(workspace, dendrogram, c),
        view(stack, strt:stop),
    )
    return strt
end

function apply_root!(
        workspace::Workspace,
        algebra::AbstractAlgebra{A},
        dendrogram::AbstractDendrogram{I, L},
        arguments,
        stack::AbstractVector{A},
        strt::I,
    ) where {A, I <: Integer, L}

    stop = strt

    for b in boxes(dendrogram, nob(dendrogram))
        stop += one(I); stack[stop] = arguments[b]
    end

    return apply(
        algebra,
        WiringDiagram(workspace, dendrogram),
        view(stack, oneto(stop)),
    )
end
