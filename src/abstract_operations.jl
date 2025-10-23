"""
    AbstractOperation{I, L}

A morphism in a multi-category.
"""
abstract type AbstractOperation{I <: Integer, L} end

function Base.show(io::IO, operation::AbstractOperation)
    n = arity(operation)

    for i in oneto(n)
        if isone(i)
            print(io, domain(operation, i))
        elseif i <= MAX_ITEMS_PRINTED || i == n == MAX_ITEMS_PRINTED + 1
            print(io, ", ")
            print(io, domain(operation, i))
        else
            print(io, ", …, ")
            print(io, domain(operation, n))
            break
        end
    end

    print(io, " → ")
    print(io, codomain(operation))
    return
end

function Base.show(io::IO, ::MIME"text/plain", operation::Opn) where {Opn <: AbstractOperation}
    n = arity(operation)

    print(io, n)
    print(io, "-ary ")
    print(io, Opn)
    print(io, ":\n ")
    print(io, codomain(operation))

    for i in oneto(n)
        if i <= MAX_ITEMS_PRINTED || i == n == MAX_ITEMS_PRINTED + 1
            print(io, "\n └─ ")
            print(io, domain(operation, i))
        else
            print(io, "\n  ⋮")
            print(io, "\n └─ ")
            print(io, domain(operation, n))
            break
        end
    end

    return
end

# ---------------------------- #
# Abstract Operation Interface #
# ---------------------------- #

"""
    arity(operation::AbstractOperation)

Get the arity of an operation.
"""
arity(operation::AbstractOperation)

"""
    eachargument(operation::AbstractOperation)

Get the ordered set

    {1, ..., n} ⊆ ℕ

where n is the arity of the operation.
"""
eachargument(operation::AbstractOperation)

"""
    domain(operation::AbstractOperation)

Get the domain of an operation.
"""
function domain(operation::AbstractOperation)
    dom = map(eachargument(operation)) do i
        return domain(operation, i)
    end

    return dom
end

"""
    domain(operation::AbstractOperation, i::Integer)

Get the ith element of the domain of an operation.
"""
domain(operation::AbstractOperation, i::Integer)

"""
    codomain(operation::AbstractOperation)

Get the codomain of an operation.
"""
codomain(operation::AbstractOperation)

"""
    compose(i, outer::AbstractOperation, inner)
"""
function compose(i, outer::AbstractOperation, inner)
    diagram = foldr(zip(i, inner); init=outer) do (i, inner), outer
        return compose(i, outer, inner)
    end

    return diagram
end

"""
    compose(i::Integer, outer::AbstractOperation, inner::AbstractOperation)

Given operations

    inner: (A₁, …, Aₘ) → Bᵢ
    outer: (B₁, …, Bₙ) → C

form the composite operation

    outer ∘ᵢ inner: (B₁, …, Bᵢ₋₁, A₁, …, Aₘ, Bᵢ₊₁, …, Bₙ) → C 

given by the following string diagram:

                      ┌───────┐
    B₁ ───────────────│       │
     ⋮                │       │
    Bᵢ₋₁ ─────────────│       │
          ┌───────┐   │       │
    A₁ ───│       │ Bᵢ│       │
     ⋮    │ inner │───│ outer │─── C
    Aₘ ───│       │   │       │
          └───────┘   │       │
    Bᵢ₊₁ ─────────────│       │
     ⋮                │       │
    Bₙ ───────────────│       │
                      └───────┘

"""
compose(i::Integer, outer::AbstractOperation, inner::AbstractOperation)
