"""
    AbstractDendrogram{I, L}

A labelled dendrogram is a commutative diagram in ``\\text{Set}`` of the form

```text
+ -------------------- +
|         L label      |
|    wire ↑ outbox     |
|     P → W ← Q        |
| box ↓   ↓   ↓ outbox |
|     B → C ← C        |
|  parent  outparent   |
+ -------------------- +
```

where

  - ``B`` is a set of boxes
  - ``C`` is a set of outer boxes
  - ``W`` is a set of wires
  - ``P`` is a set of ports
  - ``Q`` is a set of outer ports
  - ``L`` is a set of labels

The functions `parent` and `outparent` define a rooted tree
with leaves ``B`` and branches ``C``.

```text
+ --------------- +
|         c       |
|         ↓       |
|         c       |
|       ↙ ↓  ↘    |
|     c   c    b  |
|   ↙   ↘   ↘     |
|  b     b    b   |
+ --------------- +
```

The sets ``B``, ``C``, ``W``, ``P``, and ``Q`` are contiguous sets of natural numbers

  - ``B = \\{1, \\ldots, |B|\\}``
  - ``C = \\{1, \\ldots, |C|\\}``
  - ``W = \\{1, \\ldots, |W|\\}``
  - ``P = \\{1, \\ldots, |P|\\}``
  - ``Q = \\{1, \\ldots, |Q|\\}``

and the set ``L`` is an arbitrary Julia type like `Symbol` or `Int`. The functions

  - ``\\text{box}: P \\to B``
  - ``\\text{outbox}: Q \\to C``
  - ``\\text{wirebox}: W \\to C``

are monotonically increasing, so that for all boxes ``b \\in B`` and outer boxes
``c \\in C``, the pre-images

  - ``\\text{box}^{-1}(b) = \\{p \\in P : \\text{box}(p) = b\\} \\subseteq P``
  - ``\\text{outbox}^{-1}(c) = \\{q \\in Q : \\text{outbox}(q) = c\\} \\subseteq Q``
  - ``\\text{wirebox}^{-1}(c) = \\{w \\in W : \\text{wirebox}(w) = c\\} \\subseteq W``

are contiguous sets of natural numbers:

  - ``\\text{box}^{-1}(b) = \\{p, \\ldots, p + |\\text{box}^{-1}(b)| - 1\\} \\subseteq P``
  - ``\\text{outbox}^{-1}(c) = \\{q, \\ldots, q + |\\text{outbox}^{-1}(c)| - 1\\} \\subseteq Q``
  - ``\\text{wirebox}^{-1}(c) = \\{w, \\ldots, w + |\\text{wirebox}^{-1}(c)| - 1\\} \\subseteq W``

Hence, there are functions `port`, `outport`, and `boxwire`
which map each box ``b \\in B``, outer box ``c \\in C``, and numbers

  - ``1 \\leq i \\leq |\\text{box}^{-1}(b)|``
  - ``1 \\leq j \\leq |\\text{outbox}^{-1}(c)|``
  - ``1 \\leq k \\leq |\\text{wirebox}^{-1}(c)|``

to the objects

  - ``\\text{port}(b, i) := p + i - 1 \\in \\text{box}^{-1}(b)``
  - ``\\text{outport}(c, j) := q + j - 1 \\in \\text{outbox}^{-1}(c)``
  - ``\\text{boxwire}(c, k) := w + k - 1 \\in \\text{wirebox}^{-1}(c)``
"""
abstract type AbstractDendrogram{I, L} <: AbstractOperation{I, L} end

"""
    treewidth([f::Function,] dendrogram::AbstractDendrogram)
"""
function CliqueTrees.treewidth(dendrogram::AbstractDendrogram)
    return treewidth(uniformweight, dendrogram)
end

function CliqueTrees.treewidth(f::Function, dendrogram::AbstractDendrogram)
    maxwidth = nothing

    for c in outboxes(dendrogram)
        width = nothing

        for l in wirelabels(dendrogram, c)
            if !isnothing(width)
                width += f(l)
            else
                width = f(l)
            end
        end

        if !isnothing(width)
            if !isnothing(maxwidth)
                maxwidth = max(maxwidth, width)
            else
                maxwidth = width
            end
        end
    end

    return maxwidth
end

"""
    separatorwidth([f::Function,] dendrogram::AbstractDendrogram)
"""
function CliqueTrees.separatorwidth(dendrogram::AbstractDendrogram)
    return separatorwidth(uniformweight, dendrogram)
end

function CliqueTrees.separatorwidth(f::Function, dendrogram::AbstractDendrogram)
    maxwidth = nothing

    for b in boxes(dendrogram)  
        width = nothing

        for l in portlabels(dendrogram, b)
            if !isnothing(width)
                width += f(l)
            else
                width = f(l)
            end
        end

        if !isnothing(width)
            if !isnothing(maxwidth)
                maxwidth = max(maxwidth, width)
            else
                maxwidth = width
            end
        end
    end

    for c in outboxes(dendrogram)
        width = nothing

        for l in outportlabels(dendrogram, c)
            if !isnothing(width)
                width += f(l)
            else
                width = f(l)
            end
        end

        if !isnothing(width)
            if !isnothing(maxwidth)
                maxwidth = max(maxwidth, width)
            else
                maxwidth = width
            end
        end
    end

    return maxwidth
end

# ---------------------------- #
# Abstract Operation Interface #
# ---------------------------- #

function arity(dendrogram::AbstractDendrogram)
    return nb(dendrogram)
end

function eachargument(dendrogram::AbstractDendrogram)
    return boxes(dendrogram)
end

function domain(dendrogram::AbstractDendrogram, b::Integer)
    return portlabels(dendrogram, b)
end

function domain(dendrogram::AbstractDendrogram{<:Any, Nothing}, b::Integer)
    return np(dendrogram, b)
end

function codomain(dendrogram::AbstractDendrogram)
    return outportlabels(dendrogram, nob(dendrogram))
end

function codomain(dendrogram::AbstractDendrogram{<:Any, Nothing})
    return nop(dendrogram, nob(dendrogram))
end

# ----------------------------- #
# Abstract Dendrogram Interface #
# ----------------------------- #

"""
    nb(dendrogram::AbstractDendrogram)

Get the number of boxes in a dendrogram:

```math
|B|.
```
"""
nb(dendrogram::AbstractDendrogram)

"""
    nob(dendrogram::AbstractDendrogram)

Get the number of outer boxes in a dendrogram:

```math
|C|.
```
"""
nob(dendrogram::AbstractDendrogram)

"""
    nw(dendrogram::AbstractDendrogram)

Get the number of wires in a dendrogram:

```math
|W|.
```
"""
nw(dendrogram::AbstractDendrogram)

"""
    np(dendrogram::AbstractDendrogram)

Get the number of ports in a dendrogram:

```math
|P|.
```
"""
np(dendrogram::AbstractDendrogram)

"""
    nop(dendrogram::AbstractDendrogram)

Get the number of outer ports in a dendrogram:

```math
|Q|.
```
"""
nop(dendrogram::AbstractDendrogram)

"""
    boxes(dendrogram::AbstractDendrogram)

Get the ordered set of boxes in a dendrogram:

```math
B.
```
"""
boxes(dendrogram::AbstractDendrogram)

"""
    outboxes(dendrogram::AbstractDendrogram)

Get the ordered set of outer boxes in a dendrogram:

```math
C.
```
"""
outboxes(dendrogram::AbstractDendrogram)

"""
    wires(dendrogram::AbstractDendrogram)

Get the ordered set of wires in a dendrogram:

```math
W.
```
"""
wires(dendrogram::AbstractDendrogram)

"""
    wirelabels(dendrogram::AbstractDendrogram)

Get the function ``W \\to L``:

```math
\\text{wirelabels}(w) := \\text{label}(w).
```
"""
wirelabels(dendrogram::AbstractDendrogram)

"""
    ports(dendrogram::AbstractDendrogram)

Get the ordered set of ports in a dendrogram:

```math
P.
```
"""
ports(dendrogram::AbstractDendrogram)

"""
    outports(dendrogram::AbstractDendrogram)

Get the ordered set of outer ports in a dendrogram:

```math
Q.
```
"""
outports(dendrogram::AbstractDendrogram)

"""
    np(dendrogram::AbstractDendrogram, b::Integer)

Get the size of the pre-image ``\\text{box}^{-1}(b) \\subseteq P``:

```math
|\\{p \\in P : \\text{box}(p) = b\\}|.
```
"""
np(dendrogram::AbstractDendrogram, b::Integer)

"""
    ports(dendrogram::AbstractDendrogram, b::Integer)

Get the ordered pre-image ``\\text{box}^{-1}(b) \\subseteq P``:

```math
\\{p \\in P : \\text{box}(p) = b\\} \\subseteq P.
```
"""
ports(dendrogram::AbstractDendrogram, b::Integer)

"""
    portwires(dendrogram::AbstractDendrogram, b::Integer)

Get the composite function ``\\{1, \\ldots, |\\text{box}^{-1}(b)|\\} \\to W``:

```math
\\text{portwires}(i) := \\text{wire}(\\text{port}(b, i)).
```
"""
portwires(dendrogram::AbstractDendrogram, b::Integer)

"""
    portlabels(dendrogram::AbstractDendrogram, b::Integer)

Get the composite function ``\\{1, \\ldots, |\\text{box}^{-1}(b)|\\} \\to L``:

```math
\\text{portlabels}(i) := \\text{label}(\\text{wire}(\\text{port}(b, i))).
```
"""
portlabels(dendrogram::AbstractDendrogram, b::Integer)

"""
    parent(dendrogram::AbstractDendrogram, b::Integer)

Get the parent of a box ``b \\in B``:

```math
\\text{parent}(b) \\in C.
```
"""
parent(dendrogram::AbstractDendrogram, b::Integer)

"""
    nop(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image ``\\text{outbox}^{-1}(c) \\subseteq Q``:

```math
|\\{q \\in Q : \\text{outbox}(q) = c\\}|.
```
"""
nop(dendrogram::AbstractDendrogram, c::Integer)

"""
    outports(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image ``\\text{outbox}^{-1}(c) \\subseteq Q``:

```math
\\{q \\in Q : \\text{outbox}(q) = c\\} \\subseteq Q.
```
"""
outports(dendrogram::AbstractDendrogram, c::Integer)

"""
    outportwires(dendrogram::AbstractDendrogram, c::Integer)

Get the composite function ``\\{1, \\ldots, |\\text{outbox}^{-1}(c)|\\} \\to W``:

```math
\\text{outportwires}(i) := \\text{outwire}(\\text{outport}(c, i)).
```
"""
outportwires(dendrogram::AbstractDendrogram, c::Integer)

"""
    outportlabels(dendrogram::AbstractDendrogram, c::Integer)

Get the composite function ``\\{1, \\ldots, |\\text{outbox}^{-1}(c)|\\} \\to L``:

```math
\\text{outportlabels}(i) := \\text{label}(\\text{outwire}(\\text{outport}(c, i))).
```
"""
outportlabels(dendrogram::AbstractDendrogram, c::Integer)

"""
    outparent(dendrogram::AbstractDendrogram, c::Integer)

Get the parent of an outer box ``c \\in C``:

```math
\\text{outparent}(c) \\in C.
```
"""
outparent(dendrogram::AbstractDendrogram, c::Integer)

"""
    nb(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image ``\\text{parent}^{-1}(c) \\subseteq B``:

```math
|\\{b \\in B : \\text{parent}(b) = c\\}|.
```
"""
nb(dendrogram::AbstractDendrogram, c::Integer)

"""
    boxes(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image ``\\text{parent}^{-1}(c) \\subseteq B``:

```math
\\{b \\in B : \\text{parent}(b) = c\\} \\subseteq B.
```
"""
boxes(dendrogram::AbstractDendrogram, c::Integer)

"""
    nob(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image ``\\text{outparent}^{-1}(c) \\subseteq C``:

```math
|\\{c' \\in C : \\text{outparent}(c') = c\\}|.
```
"""
nob(dendrogram::AbstractDendrogram, c::Integer)

"""
    outboxes(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image ``\\text{outparent}^{-1}(c) \\subseteq C``:

```math
\\{c' \\in C : \\text{outparent}(c') = c\\} \\subseteq C.
```
"""
outboxes(dendrogram::AbstractDendrogram, c::Integer)

"""
    nw(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image ``\\text{wirebox}^{-1}(c) \\subseteq W``:

```math
|\\{w \\in W : \\text{wirebox}(w) = c\\}|.
```
"""
nw(dendrogram::AbstractDendrogram, c::Integer)

"""
    wires(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image ``\\text{wirebox}^{-1}(c) \\subseteq W``:

```math
\\{w \\in W : \\text{wirebox}(w) = c\\} \\subseteq W.
```
"""
wires(dendrogram::AbstractDendrogram, c::Integer)

"""
    wirelabels(dendrogram::AbstractDendrogram, c::Integer)

Get the composite function ``\\{1, \\ldots, |\\text{wirebox}^{-1}(c)|\\} \\to L``:

```math
\\text{wirelabels}(i) := \\text{label}(\\text{boxwire}(c, i)).
```
"""
wirelabels(dendrogram::AbstractDendrogram, c::Integer)

"""
    label(dendrogram::AbstractDendrogram, w::Integer)

Get the label of a wire ``w \\in W``:

```math
\\text{label}(w) \\in L.
```
"""
label(dendrogram::AbstractDendrogram, w::Integer)

"""
    wire(dendrogram::AbstractDendrogram, p::Integer)

Get the wire of a port ``p \\in P``:

```math
\\text{wire}(p) \\in W.
```
"""
wire(dendrogram::AbstractDendrogram, p::Integer)

"""
    portlabel(dendrogram::AbstractDendrogram, p::Integer)

Get the label of the wire of a port ``p \\in P``:

```math
\\text{label}(\\text{wire}(p)) \\in L.
```
"""
portlabel(dendrogram::AbstractDendrogram, p::Integer)

"""
    outwire(dendrogram::AbstractDendrogram, q::Integer)

Get the wire of an outer port ``q \\in Q``:

```math
\\text{outwire}(q) \\in W.
```
"""
outwire(dendrogram::AbstractDendrogram, q::Integer)

"""
    outportlabel(dendrogram::AbstractDendrogram, q::Integer)

Get the label of the wire of an outer port ``q \\in Q``:

```math
\\text{label}(\\text{outwire}(q)) \\in L.
```
"""
outportlabel(dendrogram::AbstractDendrogram, q::Integer)
