"""
    AbstractDendrogram{I, L}

A labelled dendrogram is a commutative dendrogram in Set of the form

    + -------------------- +
    |         L label      |
    |    wire ↑ outbox     |
    |     P → W ← Q        |
    | box ↓   ↓   ↓ outbox |
    |     B → C ← C        |
    |  parent  outparent   |
    + -------------------- +

where

  - B is a set of boxes
  - C is a set of outer boxes
  - W is a set of wires
  - P is a set of ports
  - Q is a set of outer ports
  - L is a set of labels

The functions parent and outparent define a rooted tree
with leaves B and branches C.

    + --------------- +
    |         c       |
    |         ↓       |
    |         c       |
    |       ↙ ↓  ↘    |
    |     c   c    b  |
    |   ↙   ↘   ↘     |
    |  b     b    b   |
    + --------------- +

The sets B, C, W, P, and Q are contiguous sets of natural numbers

  - B = {1, ..., |B|}
  - C = {1, ..., |C|}
  - W = {1, ..., |W|}
  - P = {1, ..., |P|}
  - Q = {1, ..., |Q|}

and the set L is an arbitrary Julia type like Symbol or Int. The functions

  - box: P → B
  - outbox: Q → C
  - wirebox: W → C

are monotonically increasing, to that for all boxes b ∈ B and outer boxes
c ∈ C, the pre-images

  - box⁻¹(b) = {p ∈ P : box(p) = b} ⊆ P
  - outbox⁻¹(c) = {q ∈ Q : outbox(q) = c} ⊆ Q 
  - wirebox⁻¹(c) = {w ∈ W : wirebox(w) = c} ⊆ W

are contiguos sets of natural numbers:

  - box⁻¹(b) = {p, ..., p + |box⁻¹(b)| - 1} ⊆ P
  - outbox⁻¹(c) = {q, ..., q + |outbox⁻¹(c)| - 1} ⊆ Q
  - wirebox⁻¹(c) = {w, ..., w + |wirebox⁻¹(c)| - 1} ⊆ W

Hence, there are functions port, outport, and boxwire
which map each box b ∈ B, outer box c ∈ C, and numbers

  - 1 ≤ i ≤ |box⁻¹(b)|
  - 1 ≤ j ≤ |outbox⁻¹(c)|
  - 1 ≤ k ≤ |wirebox⁻¹(c)|

to the objects

  - port(b, i) := p + i - 1 ∈ box⁻¹(b)
  - outport(q, j) := q + j - 1 ∈ outbox⁻¹(c)
  - boxwire(w, k) := w + k - 1 ∈ wirebox⁻¹(c)

"""
abstract type AbstractDendrogram{I, L} <: AbstractOperation{I, L} end

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

    |B|.

"""
nb(dendrogram::AbstractDendrogram)

"""
    nob(dendrogram::AbstractDendrogram)

Get the number of outer boxes in a dendrogram:

    |C|.

"""
nob(dendrogram::AbstractDendrogram)

"""
    nw(dendrogram::AbstractDendrogram)

Get the number of wires in a dendrogram:

    |W|.

"""
nw(dendrogram::AbstractDendrogram)

"""
    np(dendrogram::AbstractDendrogram)

Get the number of ports in a dendrogram:

    |P|.

"""
np(dendrogram::AbstractDendrogram)

"""
    nop(dendrogram::AbstractDendrogram)

Get the number of outer ports in a dendrogram:

    |Q|.

"""
nop(dendrogram::AbstractDendrogram)

"""
    boxes(dendrogram::AbstractDendrogram)

Get the ordered set of boxes in a dendrogram:

    B.

"""
boxes(dendrogram::AbstractDendrogram)

"""
    outboxes(dendrogram::AbstractDendrogram)

Get the orderd set of outer boxes in a dendrogram:

    C.

"""
outboxes(dendrogram::AbstractDendrogram)

"""
    wires(dendrogram::AbstractDendrogram)

Get the ordered set of wires in a dendrogram:

    W.
"""
wires(dendrogram::AbstractDendrogram)

"""
    wirelabels(dendrogram::AbstractDendrogram)

Get the function of W → L:

    wirelabels(w) := label(w).

"""
wirelabels(dendrogram::AbstractDendrogram)

"""
    ports(dendrogram::AbstractDendrogram)

Get the ordered set of ports in a dendrogram:

   P.

"""
ports(dendrogram::AbstractDendrogram)

"""
    outports(dendrogram::AbstractDendrogram)

Get the ordered set of outer ports in a dendrogram:

    Q.

"""
outports(dendrogram::AbstractDendrogram)

"""
    ports(dendrogram::AbstractDendrogram, b::Integer)

Get the size of the pre-image box⁻¹(b) ⊆ P:

    | {p ∈ P : box(p) = b} |.

"""
np(dendrogram::AbstractDendrogram, b::Integer)

"""
    ports(dendrogram::AbstractDendrogram, b::Integer)

Get the ordered pre-image box⁻¹(b) ⊆ P:

    {p ∈ P : box(p) = b} ⊆ P.

"""
ports(dendrogram::AbstractDendrogram, b::Integer)

"""
    portwires(dendrogram::AbstractDendrogram, b::Integer)

Get the composite function {1, ..., |box⁻¹(b)|} → W:

    portwires(i) := wire(port(b, i)).

"""
portwires(dendrogram::AbstractDendrogram, b::Integer)

"""
    portlabels(dendrogram::AbstractDendrogram, b::Integer)

Get the composite function {1, ..., |box⁻¹(b)|} → L:

    portlabels(i) := label(wire(port(b, i))).

"""
portlabels(dendrogram::AbstractDendrogram, b::Integer)

"""
    parent(dendrogram::AbstractDendrogram)

Get the parent of a box b ∈ B:

    parent(b) ∈ C.

"""
parent(dendrogram::AbstractDendrogram, b::Integer)

"""
    nop(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image outbox⁻¹(c) ⊆ Q:

    | {q ∈ Q : outbox(q) = c} |.

"""
nop(dendrogram::AbstractDendrogram, c::Integer)

"""
    outports(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image outbox⁻¹(c) ⊆ Q:

    {q ∈ Q : outbox(q) = c} ⊆ Q.

"""
outports(dendrogram::AbstractDendrogram, c::Integer)

"""
    outportwires(dendrogram::AbstractDendrogram, c::Integer)

Get the composite function {1, ..., |outbox⁻¹(c)|} → W:

    outportwires(i) := outwire(outport(c, i)).

"""
outportwires(dendrogram::AbstractDendrogram, c::Integer)

"""
    outportlabels(dendrogram::AbstractDendrogram, c::Integer)

Get the composite function {1, ..., |outbox⁻¹(c)|} → L:

    outportlabels(i) := label(outwire(outport(c, i))).

"""
outportlabels(dendrogram::AbstractDendrogram, c::Integer)

"""
    outparent(dendrogram::AbstractDendrogram, c::Integer)

Get the parent of an outer box c ∈ C:

    outparent(c) ∈ C.

"""
outparent(dendrogram::AbstractDendrogram, c::Integer)

"""
    nb(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image parent⁻¹(c) ⊆ B:

    | {b ∈ B : parent(b) = c} |.

"""
nb(dendrogram::AbstractDendrogram, c::Integer)

"""
    boxes(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image parent⁻¹(c) ⊆ B:

    {b ∈ B : parent(b) = c} ⊆ B.

"""
boxes(dendrogram::AbstractDendrogram, c::Integer)

"""
    nob(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image outparent⁻¹(c) ⊆ C:

    | {cc ∈ C : outparent(cc) = c} |.

"""
nob(dendrogram::AbstractDendrogram, c::Integer)

"""
    outboxes(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image outparent⁻¹(c) ⊆ C:

    {cc ∈ C : outparent(cc) = c} ⊆ C.

"""
outboxes(dendrogram::AbstractDendrogram, c::Integer)

"""
    nw(dendrogram::AbstractDendrogram, c::Integer)

Get the size of the pre-image wirebox⁻¹(c) ⊆ W:

    | {w ∈ W : wirebox(w) = c} |.

"""
nw(dendrogram::AbstractDendrogram, c::Integer)

"""
    wires(dendrogram::AbstractDendrogram, c::Integer)

Get the ordered pre-image wirebox⁻¹(c) ⊆ W:

    {w ∈ W : wirebox(w) = c} ⊆ W.

"""
wires(dendrogram::AbstractDendrogram, c::Integer)

"""
    wirelabels(dendrogram::AbstractDendrogram, c::Integer)

Get the composite function {1, ..., |wirebox⁻¹(c)|} → L:

    wirelabels(i) := label(boxwire(c, i))

"""
wirelabels(dendrogram::AbstractDendrogram, c::Integer)

"""
    label(dendrogram::AbstractDendrogram, w::Integer)

Get the label of a wire w ∈ W:

    label(w) ∈ L.

"""
label(dendrogram::AbstractDendrogram, w::Integer)

"""
    wire(dendrogram::AbstractDendrogram, p::Integer)

Get the wire of a port p ∈ P:

    wire(p) ∈ W.

"""
wire(dendrogram::AbstractDendrogram, p::Integer)

"""
    portlabel(dendrogram::AbstractDendrogram, p::Integer)

Get the label of the wire of a port p ∈ P:

    label(wire(p)) ∈ L.

"""
portlabel(dendrogram::AbstractDendrogram, p::Integer)

"""
    outwire(dendrogram::AbstractDendrogram, q::Integer)

Get the wire of an outer port q ∈ O:

    outwire(q) ∈ W.

"""
outwire(dendrogram::AbstractDendrogram, q::Integer)

"""
    outportlabel(dendrogram::AbstractDendrogram, q::Integer)

Get the label of the wire of an outer port q ∈ Q:

    label(outwire(q)) ∈ L.

"""
outportlabel(dendrogram::AbstractDendrogram, q::Integer)
