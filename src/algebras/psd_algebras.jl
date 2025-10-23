struct PSDAlgebra{M <: AbstractMatrix} <: AbstractAlgebra{M} end

function apply(algebra::PSDAlgebra{M}, diagram::AbstractWiringDiagram{I}, arguments) where {T, M <: AbstractMatrix{T}, I <: Integer}
    W = nw(diagram)
    Q = nop(diagram)
    R = W - Q

    WW = M(undef, W, W)
    RR = view(WW, oneto(R),     oneto(R))
    RQ = view(WW, oneto(R),     R + one(I):W)
    QQ = view(WW, R + one(I):W, R + one(I):W)    

    f = FVector{I}(undef, W)

    for w in wires(diagram)
        f[w] = zero(I)

        for ww in wires(diagram)
            WW[ww, w] = zero(I)
        end
    end

    ww = R

    for w in outportwires(diagram)
        if iszero(f[w])
            f[w] = ww += one(I)
        else
            error()
        end
    end

    ww = zero(I)

    for w in wires(diagram)
        if iszero(f[w])
            f[w] = ww += one(I)
        end
    end

    for b in boxes(diagram)
        p0 = first(ports(diagram, b)) - one(I)
        PP = arguments[b]

        for p in ports(diagram, b)
            w = wire(diagram, p); fw = f[w]

            for pp in ports(diagram, b)
                ww = wire(diagram, pp); fww = f[ww]
                WW[fww, fw] += PP[pp - p0, p - p0]
            end
        end
    end

    if isone(R)
        rr = sqrt(only(RR))
        rq = reshape(RQ, Q); rq ./= rr
        BLAS.syr!('L', -one(T), rq, QQ)
    elseif ispositive(R)
        cholesky!(Symmetric(RR, :L))
        ldiv!(LowerTriangular(RR), RQ)
        BLAS.syrk!('L', 'T', -one(T), RQ, one(T), QQ)
    end

    return Symmetric(QQ, :L)    
end
