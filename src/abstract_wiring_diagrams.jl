"""
    AbstractWiringDiagram{I, L} <: AbstractOperation{I, L}

A labelled wiring diagram is a diagram in ``\\text{Set}`` of the form

```text
+ --------------- +
|       B         |
|       ↑ box     |
|       P         |
| label ↓ wire    |
|   L ← W         |
|       ↑ outwire |
|       Q         |
+ --------------- +
```

where

  - ``B`` is a set of boxes
  - ``W`` is a set of wires
  - ``P`` is a set of ports
  - ``Q`` is a set of outer ports
  - ``L`` is a set of labels

The sets ``B``, ``W``, ``P``, and ``Q`` are contiguous sets of natural numbers

  - ``B = \\{1, \\ldots, |B|\\}``
  - ``W = \\{1, \\ldots, |W|\\}``
  - ``P = \\{1, \\ldots, |P|\\}``
  - ``Q = \\{1, \\ldots, |Q|\\}``

and the set ``L`` is an arbitrary Julia type like `Symbol` or `Int`. The
function ``\\text{box}: P \\to B`` is monotonically increasing, so that for all
boxes ``b \\in B``, the pre-image

```math
\\text{box}^{-1}(b) = \\{p \\in P : \\text{box}(p) = b\\} \\subseteq P
```

is a contiguous set of natural numbers

```math
\\text{box}^{-1}(b) = \\{p, \\ldots, p + |\\text{box}^{-1}(b)| - 1\\} \\subseteq P.
```

Hence, there is a function `port` which maps each
box ``b \\in B`` and number ``1 \\leq i \\leq |\\text{box}^{-1}(b)|`` to the
port

```math
\\text{port}(b, i) := p + i - 1 \\in \\text{box}^{-1}(b).
```
"""
abstract type AbstractWiringDiagram{I, L} <: AbstractOperation{I, L} end

function CliqueTrees.BipartiteGraph(diagram::AbstractWiringDiagram{I}) where {I <: Integer}
    B = nb(diagram)
    W = nw(diagram)
    P = np(diagram)
    Q = nop(diagram)

    marker = FVector{I}(undef, W)
    pointer = FVector{I}(undef, B + two(I))
    target = FVector{I}(undef, P + Q)

    for w in wires(diagram)
        marker[w] = zero(I)
    end

    p = zero(I)

    for b in boxes(diagram)
        pointer[b] = p + one(I)

        for w in portwires(diagram, b)
            if marker[w] < b
                p += one(I); marker[w] = b; target[p] = w
            end
        end
    end

    pointer[B + one(I)] = p + one(I)

    for w in outportwires(diagram)
        if marker[w] < B + one(I)
            p += one(I); marker[w] = B + one(I); target[p] = w
        end
    end

    pointer[B + two(I)] = p + one(I)
    return BipartiteGraph(W, B + one(I), p, pointer, target)
end

function SparseArrays.sparse(diagram::AbstractWiringDiagram)
    graph = BipartiteGraph(diagram)
    return sparse(graph)
end

function Base.convert(::Type{Dgm}, diagram::Dgm) where {Dgm <: AbstractWiringDiagram}
    return diagram
end

function Base.convert(::Type{Dgm}, diagram::AbstractWiringDiagram) where {Dgm <: AbstractWiringDiagram}
    return Dgm(diagram)
end

# ---------------------------- #
# Abstract Operation Interface #
# ---------------------------- #

function arity(diagram::AbstractWiringDiagram)
    return nb(diagram)
end

function eachargument(diagram::AbstractWiringDiagram)
    return boxes(diagram)
end

function domain(diagram::AbstractWiringDiagram, b::Integer)
    return portlabels(diagram, b)
end

function domain(diagram::AbstractWiringDiagram{<:Any, Nothing}, b::Integer)
    return np(diagram, b)
end

function codomain(diagram::AbstractWiringDiagram)
    return outportlabels(diagram)
end

function codomain(diagram::AbstractWiringDiagram{<:Any, Nothing})
    return nop(diagram)
end

# --------------------------------- #
# Abstract Wiring Diagram Interface #
# --------------------------------- #

"""
    nb(diagram::AbstractWiringDiagram)

Get the number of boxes in a wiring diagram:

```math
|B|.
```
"""
nb(diagram::AbstractWiringDiagram)

"""
    nw(diagram::AbstractWiringDiagram)

Get the number of wires in a wiring diagram:

```math
|W|.
```
"""
nw(diagram::AbstractWiringDiagram)

"""
    np(diagram::AbstractWiringDiagram)

Get the number of ports in a wiring diagram:

```math
|P|.
```
"""
np(diagram::AbstractWiringDiagram)

"""
    nop(diagram::AbstractWiringDiagram)

Get the number of outer ports in a wiring diagram:

```math
|Q|.
```
"""
nop(diagram::AbstractWiringDiagram)

"""
    boxes(diagram::AbstractWiringDiagram)

Get the ordered set of boxes in a wiring diagram:

```math
B.
```
"""
boxes(diagram::AbstractWiringDiagram)

"""
    wires(diagram::AbstractWiringDiagram)

Get the ordered set of wires in a wiring diagram:

```math
W.
```
"""
wires(diagram::AbstractWiringDiagram)

"""
    wirelabels(diagram::AbstractWiringDiagram)

Get the function ``W \\to L``:

```math
\\text{wirelabels}(w) := \\text{label}(w).
```
"""
wirelabels(diagram::AbstractWiringDiagram)

"""
    ports(diagram::AbstractWiringDiagram)

Get the ordered set of ports in a wiring diagram:

```math
P.
```
"""
ports(diagram::AbstractWiringDiagram)

"""
    portwires(diagram::AbstractWiringDiagram)

Get the function ``P \\to W``:

```math
\\text{portwires}(p) := \\text{wire}(p).
```
"""
portwires(diagram::AbstractWiringDiagram)

"""
    outports(diagram::AbstractWiringDiagram)

Get the ordered set of outer ports in a wiring diagram:

```math
Q.
```
"""
outports(diagram::AbstractWiringDiagram)

"""
    outportwires(diagram::AbstractWiringDiagram)

Get the function ``Q \\to W``:

```math
\\text{outportwires}(q) := \\text{outwire}(q).
```
"""
outportwires(diagram::AbstractWiringDiagram)

"""
    outportlabels(diagram::AbstractWiringDiagram)

Get the composite function ``Q \\to L``:

```math
\\text{outportlabels}(q) := \\text{label}(\\text{outwire}(q)).
```
"""
outportlabels(diagram::AbstractWiringDiagram)

"""
    np(diagram::AbstractWiringDiagram, b::Integer)

Get the size of the preimage ``\\text{box}^{-1}(b) \\subseteq P``:

```math
|\\{p \\in P : \\text{box}(p) = b\\}|.
```
"""
np(diagram::AbstractWiringDiagram, b::Integer)

"""
    ports(diagram::AbstractWiringDiagram, b::Integer)

Get the ordered preimage ``\\text{box}^{-1}(b) \\subseteq P``:

```math
\\{p \\in P : \\text{box}(p) = b\\} \\subseteq P.
```
"""
ports(diagram::AbstractWiringDiagram, b::Integer)

"""
    portwires(diagram::AbstractWiringDiagram, b::Integer)

Get the composite function ``\\{1, \\ldots, |\\text{box}^{-1}(b)|\\} \\to W``:

```math
\\text{portwires}(i) := \\text{wire}(\\text{port}(b, i)).
```
"""
portwires(diagram::AbstractWiringDiagram, b::Integer)

"""
    portlabels(diagram::AbstractWiringDiagram, b::Integer)

Get the composite function ``\\{1, \\ldots, |\\text{box}^{-1}(b)|\\} \\to L``:

```math
\\text{portlabels}(i) := \\text{label}(\\text{wire}(\\text{port}(b, i))).
```
"""
portlabels(diagram::AbstractWiringDiagram, b::Integer)

"""
    label(diagram::AbstractWiringDiagram, w::Integer)

Get the label of a wire ``w \\in W``:

```math
\\text{label}(w) \\in L.
```
"""
label(diagram::AbstractWiringDiagram, w::Integer)

"""
    wire(diagram::AbstractWiringDiagram, p::Integer)

Get the wire of a port ``p \\in P``:

```math
\\text{wire}(p) \\in W.
```
"""
wire(diagram::AbstractWiringDiagram, p::Integer)

"""
    portlabel(diagram::AbstractWiringDiagram, p::Integer)

Get the label of the wire of a port ``p \\in P``:

```math
\\text{label}(\\text{wire}(p)) \\in L.
```
"""
portlabel(diagram::AbstractWiringDiagram, p::Integer)

"""
    outwire(diagram::AbstractWiringDiagram, q::Integer)

Get the wire of an outer port ``q \\in Q``:

```math
\\text{outwire}(q) \\in W.
```
"""
outwire(diagram::AbstractWiringDiagram, q::Integer)

"""
    outportlabel(diagram::AbstractWiringDiagram, q::Integer)

Get the label of the wire of an outer port ``q \\in Q``:

```math
\\text{label}(\\text{outwire}(q)) \\in L.
```
"""
outportlabel(diagram::AbstractWiringDiagram, q::Integer)

