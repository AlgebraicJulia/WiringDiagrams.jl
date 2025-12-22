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
```math
\\{1, \\ldots, n\\} \\subseteq \\mathbb{N}
```
where ``n`` is the arity of the operation.
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

Get the ``i``th element of the domain of an operation.
"""
domain(operation::AbstractOperation, i::Integer)

"""
    codomain(operation::AbstractOperation)

Get the codomain of an operation.
"""
codomain(operation::AbstractOperation)

"""
    compose(outer::AbstractOperation, inner)
"""
function compose(outer::AbstractOperation, inner)
    return compose(boxes(outer), outer, inner)
end

"""
    compose(i, outer::AbstractOperation, inner)
"""
function compose(i, outer::AbstractOperation, inner)
    @assert length(i) == length(inner)

    diagram = foldr(zip(i, inner); init=outer) do (i, inner), outer
        return compose(i, outer, inner)
    end

    return diagram
end

"""
    compose(i::Integer, outer::AbstractOperation, inner::AbstractOperation)

Given operations

  - ``\\text{inner}: (A_1, \\ldots, A_m) \\to B_i``
  - ``\\text{outer}: (B_1, \\ldots, B_n) \\to C``

form the composite operation

```math
\\text{outer} \\circ_i \\text{inner}: (B_1, \\ldots, B_{i-1}, A_1, \\ldots, A_m, B_{i+1}, \\ldots, B_n) \\to C
```

given by the following string diagram:

```text
+ -------------------------------- +
|                   ┌───────┐      |
| B_1 ──────────────│       │      |
|  ⋮                │       │      |
| B_{i-1} ──────────│       │      |
|       ┌───────┐   │       │      |
| A_1 ──│       │B_i│       │      |
|  ⋮    │ inner │───│ outer │─── C |
| A_m ──│       │   │       │      |
|       └───────┘   │       │      |
| B_{i+1} ──────────│       │      |
|  ⋮                │       │      |
| B_n ──────────────│       │      |
|                   └───────┘      |
+--------------------------------- +
```
"""
compose(i::Integer, outer::AbstractOperation, inner::AbstractOperation)
