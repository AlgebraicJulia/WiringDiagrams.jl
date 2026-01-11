struct SpanAlgebra <: AbstractAlgebra{Matrix{Int}} end

function apply(algebra::SpanAlgebra, diagram::AbstractWiringDiagram, arguments)
    return apply(algebra, diagram, ntuple(b -> arguments[b], nb(diagram)))
end

function apply(algebra::SpanAlgebra, diagram::AbstractWiringDiagram, tables::NTuple{N}) where {N}
    W = nw(diagram)
    
    table = apply_span_join(
        outportwires(diagram),
        Zeros{Int}(W),
        ntuple(n -> portwires(diagram, n), Val(N)),
        tables,
        ntuple(n -> Vector{Int}(undef, W), Val(N)),
    )

    sizes = Tuple(
        outportlabel(diagram, i)
        for i in axes(table, 1)
        if iszero(table[i, 1])
    )

    table = apply_span_extend(
        table,
        sizes,
    )

    return table
end

function apply_span_join(
        outwire::AbstractVector{Int},
        outcolumn::AbstractVector{Int},
        wires::NTuple{N, AbstractVector{Int}},
        tables::NTuple{N, AbstractMatrix{Int}},
        columns::NTuple{N, AbstractVector{Int}},
    ) where {N}
    vector = Int[]
    apply_span_join!(vector, outwire, outcolumn, wires, tables, columns)
    return reshape(vector, length(outwire), :)
end

function apply_span_join!(
        vector::AbstractVector{Int},
        outwire::AbstractVector{Int},
        outcolumn::AbstractVector{Int},
        wires::NTuple{N, AbstractVector{Int}},
        tables::NTuple{N, AbstractMatrix{Int}},
        columns::NTuple{N, AbstractVector{Int}},
    ) where {N}

    wire, wires... = wires
    table, tables... = tables
    column, columns... = columns
        
    for j in axes(table, 2)
        column .= outcolumn
        isconsistent = true
        
        @inbounds for (i, w) in enumerate(wire)
            isconsistent &= column[w] in (0, table[i, j])
            column[w] = table[i, j]
        end

        isconsistent && apply_span_join!(vector, outwire, column, wires, tables, columns)
    end

    return
end

function apply_span_join!(
        vector::AbstractVector{Int},
        outwire::AbstractVector{Int},
        outcolumn::AbstractVector{Int},
        wires::Tuple{},
        tables::Tuple{},
        columns::Tuple{},
    )

    @inbounds for w in outwire
        push!(vector, outcolumn[w])
    end
    
    return
end

function apply_span_extend(
        table::AbstractMatrix{Int},
        sizes::Tuple,
    )
    
    vector = Int[]

    for index in CartesianIndices(sizes), j in axes(table, 2)
        k = 0
        
        for i in axes(table, 1)
            Tij = table[i, j]
            
            if iszero(Tij)
                k += 1; Tij = index[k]
            end
            
            push!(vector, Tij)
        end
    end

    return reshape(vector, size(table, 1), :)
end

function apply_span_extend(
        table::AbstractMatrix{Int},
        sizes::Tuple{},
    )
    return table
end
