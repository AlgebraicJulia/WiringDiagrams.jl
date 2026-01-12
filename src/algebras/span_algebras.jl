struct SpanAlgebra <: AbstractAlgebra{Matrix{Int}} end

function apply(algebra::SpanAlgebra, diagram::AbstractWiringDiagram, arguments)
    return apply(algebra, diagram, Tuple(arguments))
end

function apply(algebra::SpanAlgebra, diagram::AbstractWiringDiagram, tables::NTuple{N}) where {N}
    return span_extend(diagram, span_join(diagram, tables))  
end

function span_join(diagram::AbstractWiringDiagram, tables::NTuple{N}) where {N}
    m = nw(diagram)
    outtable = Int[]
    outschema = outportwires(diagram)
    outrecord = Zeros{Int}(m)
    schemas = ntuple(n -> portwires(diagram, n), Val(N))
    records = ntuple(n -> Vector{Int}(undef, m), Val(N))
    return span_join!(outtable, outschema, outrecord, tables, schemas, records)
end

# Evaluate a query.
#
# ## The Query
#
# A table is a collection of records. Given a sequence (T₁, ..., Tₙ) of tables,
# this function constructs an output table T₀ by enumerating all sequences
# (r₁, ..., rₙ) ∈ T₁ × ⋯ × Tₙ of records. For each sequence, the function executes
# the following instructions.
#
#   1. Determine if the tuple (r₁, ..., rₙ) satisfies the join condition.
#   2. If the above is true, then project the tuple onto an output record.
#
# ## The Join Condition
#
# Each table Tᵢ has a corresponding schema vector sᵢ. A tuple (r₁, ..., rₙ) of
# records satisfies the query's join condition if
#
#     sᵢ[u] = sⱼ[v] → rᵢ[u] = rⱼ[v]
#
# for all tables Tᵢ and Tⱼ and columns u and v.
#
# ## Projection
#
# If a tuple (r₁, ..., rₙ) satisfies the join condition, then it is projected
# onto an output record r₀, which is appended to the output table T₀. The
# projection operation is determined by the output schema vector s₀.
#
#     s₀[u] = sⱼ[v] → r₀[u] = rⱼ[v]
#
# for all tables Tⱼ and columns u and v.
#
# ## Arguments
#
#   - `outtable`: output table (T₀)
#   - `outschema`: output schema vector (s₀)
#   - `tables`: tables (T₁, ..., Tₙ)
#   - `schemas`: schema vectors (s₁, ..., sₙ)
#
function span_join!(
        outtable::AbstractVector{Int},
        outschema::AbstractVector{Int},
        outrecord::AbstractVector{Int},
        tables::NTuple{N, AbstractMatrix{Int}},
        schemas::NTuple{N, AbstractVector{Int}},
        records::NTuple{N, AbstractVector{Int}},
    ) where {N}

    # a table Tᵢ...
    table, tables... = tables

    # ... with schema sᵢ
    schema, schemas... = schemas

    # a tuple (r₁, ..., rᵢ₋₁) of records
    record, records... = records

    # for all records rᵢ ∈ Tᵢ...        
    for j in axes(table, 2)
        # determine if the records (r₁, ..., rᵢ) satisfy
        # the join condition
        record .= outrecord
        isconsistent = true

        @inbounds for (i, w) in enumerate(schema)
            isconsistent &= record[w] in (0, table[i, j])
            record[w] = table[i, j]
        end

        # if the join condition is satisfied, proceed
        # recursively
        isconsistent && span_join!(outtable, outschema, record, tables, schemas, records)
    end

    return reshape(outtable, length(outschema), :)
end

function span_join!(
        outtable::AbstractVector{Int},
        outschema::AbstractVector{Int},
        outrecord::AbstractVector{Int},
        tables::Tuple{},
        schemas::Tuple{},
        records::Tuple{},
    )

    @inbounds for w in outschema
        push!(outtable, outrecord[w])
    end
    
    return reshape(outtable, length(outschema), :)
end

function span_extend(diagram::AbstractWiringDiagram, table::AbstractMatrix{Int})
    mask = ones(Bool, nw(diagram))

    for w in portwires(diagram)
        mask[w] = false
    end

    schema = findall(mask)
    return span_extend(diagram, table, Tuple(schema))
end

function span_extend(diagram::AbstractWiringDiagram, table::AbstractMatrix{Int}, schema::Tuple)
    sizes = map(Fix1(outportlabel, diagram), schema)
    return span_extend(table, schema, sizes)
end

function span_extend(table::AbstractMatrix{Int}, schema::NTuple{N, Int}, sizes::NTuple{N, Int}) where {N}
    outtable = Int[]

    for j in axes(table, 2)
        record = @view table[:, j]

        for index in CartesianIndices(sizes)
            for n in 1:N
                record[schema[n]] = index[n]
            end

            append!(outtable, record)
        end
    end

    return reshape(outtable, size(table, 1), :)
end

function span_extend(table::AbstractMatrix{Int}, schema::Tuple{}, sizes::Tuple{})
    return table
end
