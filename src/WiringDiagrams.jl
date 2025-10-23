module WiringDiagrams

using AbstractTrees
using Base: @propagate_inbounds, oneto
using CliqueTrees
using CliqueTrees: PermutationOrAlgorithm, SupernodeType, UnionFind,
    DEFAULT_ELIMINATION_ALGORITHM, DEFAULT_SUPERNODE_TYPE, linegraph,
    cliquetree!, incident, arcs
using CliqueTrees.Utilities
using Graphs
using SparseArrays

import AbstractTrees: parent

export AbstractOperation, arity, domain, codomain, compose
export AbstractWiringDiagram, nb, nw, np, nop, boxes, wires, wirelabels,
    ports, portwires, outports, outportwires, outportlabels, portlabels,
    label, wire, portlabel, outwire, outportlabel
export AbstractDendrogram, nob, outboxes, outparent
export AbstractAlgebra, apply
export Workspace
export WiringDiagram, DWiringDiagram, FWiringDiagram
export StaticWiringDiagram
export Dendrogram, DDendrogram, FDendrogram
export ArrayAlgebra

function uniformweight(l)
    return 1.0
end

# -------------- #
# Abstract Types #
# -------------- #

include("abstract_operations.jl")
include("abstract_wiring_diagrams.jl")
include("abstract_dendrograms.jl")
include("abstract_algebras.jl")

# ---------- #
# Workspaces #
# ---------- #

include("workspaces.jl")

# --------------- #
# Wiring Diagrams #
# --------------- #

include("wiring_diagrams.jl")
include("static_wiring_diagrams.jl")

# ----------- #
# Dendrograms #
# ----------- #

include("dendrograms.jl")

# -------- #
# Algebras #
# -------- #

include("array_algebras.jl")

# ---------- #
# Operations #
# ---------- #

include("operations.jl")

end
