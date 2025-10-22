"""
    AbstractWiringDiagram{I, L}

A labelled wiring diagram is a diagram in Set of the form

    + --------------- +
    |       B         |
    |       ↑ box     |
    |       P         |
    | label ↓ wire    |
    |   L ← W         |
    |       ↑ outwire |
    |       Q         |
    + --------------- +

where

  - B is a set of boxes
  - W is a set of wires
  - P is a set of ports
  - Q is a set of outer ports
  - L is a set of labels

The sets B, W, P, and Q are contiguous sets of natural numbers

  - B = {1, ..., |B|}
  - W = {1, ..., |W|}
  - P = {1, ..., |P|}
  - Q = {1, ..., |Q|}
  
and the set L is an arbitrary Julia type like Symbol or Int. The
function box: P → Q is monotonically increasing, so that for all
boxes b ∈ B, the pre-image

    box⁻¹(b) = {p ∈ P : box(p) = b} ⊆ P

is a contiguous set of natural numbers

    box⁻¹(b) = {p, ..., p + |box⁻¹(b)| - 1} ⊆ P.

Hence, there is a function port which maps each
box b ∈ B and number 1 ≤ i ≤ |box⁻¹(b)| to the
port

    port(b, i) := p + i - 1 ∈ box⁻¹(b).

"""
abstract type AbstractWiringDiagram{I <: Integer, L} end

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

function Base.show(io::IO, ::MIME"text/plain", diagram::Dgm) where {Dgm <: AbstractWiringDiagram{<:Integer, Nothing}}
    B = nb(diagram)

    print(io, B)
    print(io, "-box ")
    print(io, Dgm)
    print(io, ":\n ")
    print(io, nop(diagram))

    for b in boxes(diagram)
        if b <= MAX_ITEMS_PRINTED || b == B == MAX_ITEMS_PRINTED + 1
            print(io, "\n └─ ")
            print(io, np(diagram, b))
        else
            print(io, "\n  ⋮")
            print(io, "\n └─ ")
            print(io, np(diagram, B))
            break
        end
    end

    return
end

function Base.show(io::IO, ::MIME"text/plain", diagram::Dgm) where {Dgm <: AbstractWiringDiagram}
    B = nb(diagram)

    print(io, B)
    print(io, "-box ")
    print(io, Dgm)
    print(io, ":\n ")
    print(io, outportlabels(diagram))

    for b in boxes(diagram)
        if b <= MAX_ITEMS_PRINTED || b == B == MAX_ITEMS_PRINTED + 1
            print(io, "\n └─ ")
            print(io, portlabels(diagram, b))
        else
            print(io, "\n  ⋮")
            print(io, "\n └─ ")
            print(io, portlabels(diagram, B))
            break
        end
    end

    return
end

# --------------------------------- #
# Abstract Wiring Diagram Interface #
# --------------------------------- #

"""
    nb(diagram::AbstractWiringDiagram)

Get the number of boxes in a wiring diagram:

    |B|.

"""
nb(diagram::AbstractWiringDiagram)

"""
    nw(diagram::AbstractWiringDiagram)

Get the number of wires in a wiring diagram.

    |W|.

"""
nw(diagram::AbstractWiringDiagram)

"""
    np(diagram::AbstractWiringDiagram)

Get the number of ports in a wiring diagram:

    |P|.

"""
np(diagram::AbstractWiringDiagram)

"""
    nop(diagram::AbstractWiringDiagram)

Get the number of outer ports in a wiring diagram:

    |Q|.

"""
nop(diagram::AbstractWiringDiagram)

"""
    boxes(diagram::AbstractWiringDiagram)

Get the ordered set of boxes in a wiring diagram:

    B.

"""
boxes(diagram::AbstractWiringDiagram)

"""
    wires(diagram::AbstractWiringDiagram)

Get the ordered set of wires in a wiring diagram:

    W.

"""
wires(diagram::AbstractWiringDiagram)

"""
    wirelabels(diagram::AbstractWiringDiagram)

Get the function W → L:

    wirelabels(w) := label(w)

"""
wirelabels(diagram::AbstractWiringDiagram)

"""
    ports(diagram::AbstractWiringDiagram)

Get the ordered set of ports in a wiring diagram:

    P.

"""
ports(diagram::AbstractWiringDiagram)

"""
    portwires(diagram::AbstractWiringDiagram)

Get the function P → W:

    portwires(p) := wire(p)

"""
portwires(diagram::AbstractWiringDiagram)

"""
    outports(diagram::AbstractWiringDiagram)

Get the ordered set of outer ports in a wiring diagram:

    Q.

"""
outports(diagram::AbstractWiringDiagram)

"""
    outportwires(diagram::AbstractWiringDiagram)

Get the function Q → W:

    outportwires(q) := outwire(q)

"""
outportwires(diagram::AbstractWiringDiagram)

"""
    outlabels(diagram::AbstractWiringDiagram)

Get the composite function Q → L:

    outlabels(q) := label(outwire(q))

"""
outportlabels(diagram::AbstractWiringDiagram)

"""
    np(diagram::AbstractWiringDiagram, b::Integer)

Get the size of the preimage box⁻¹(b) ⊆ P:

    | {p ∈ P : box(p) = b} |.

"""
np(diagram::AbstractWiringDiagram, b::Integer)

"""
    ports(diagram::AbstractWiringDiagram, b::Integer)

Get the ordered preimage box⁻¹(b) ⊆ P:

    {p ∈ P : box(p) = b} ⊆ P.

"""
ports(diagram::AbstractWiringDiagram, b::Integer)

"""
    portwires(diagram::AbstractWiringDiagram, b::Integer)

Get the composite function {1, ..., |box⁻¹(b)|} → W:

    portwires(i) := wire(port(b, i)).

"""
portwires(diagram::AbstractWiringDiagram, b::Integer)

"""
    portlabels(diagram::AbstractWiringDiagram, b::Integer)

Get the composite function {1, ..., |box⁻¹(b)|} → L:

    portlabels(i) := label(wire(port(b, i))).

"""
portlabels(diagram::AbstractWiringDiagram, b::Integer)

"""
    label(diagram::AbstractWiringDiagram, w::Integer)

Get the label of a wire w ∈ W:

    label(w) ∈ L.

"""
label(diagram::AbstractWiringDiagram, w::Integer)

"""
    wire(diagram::AbstractWiringDiagram, p::Integer)

Get the wire of a port p ∈ P:

    wire(p) ∈ W.

"""
wire(diagram::AbstractWiringDiagram, p::Integer)

"""
    portlabel(diagram::AbstractWiringDiagram, p::Integer)

Get the label of the wire of a port p ∈ P:

    label(wire(p)) ∈ L.

"""
portlabel(diagram::AbstractWiringDiagram, p::Integer)

"""
    outwire(diagram::AbstractWiringDiagram, q::Integer)

Get the wire of an outer port q ∈ O:

    outwire(q) ∈ W.

"""
outwire(diagram::AbstractWiringDiagram, q::Integer)

"""
    outportlabel(diagram::AbstractWiringDiagram, q::Integer)

Get the label of the wire of an outer port q ∈ Q:

    label(outwire(q)) ∈ L.

"""
outportlabel(diagram::AbstractWiringDiagram, q::Integer)

