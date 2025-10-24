module WiringDiagrams

using AbstractTrees
using Base: @propagate_inbounds, oneto
using CliqueTrees
using CliqueTrees: PermutationOrAlgorithm, SupernodeType, UnionFind,
    DEFAULT_ELIMINATION_ALGORITHM, DEFAULT_SUPERNODE_TYPE, linegraph,
    cliquetree!, incident, arcs
using CliqueTrees.Utilities
using Graphs
using LinearAlgebra
using SparseArrays

import AbstractTrees: parent

export AbstractOperation, arity, domain, codomain, compose
export AbstractWiringDiagram, nb, nw, np, nop, boxes, wires, wirelabels,
    ports, portwires, outports, outportwires, outportlabels, portlabels,
    label, wire, portlabel, outwire, outportlabel
export AbstractDendrogram, nob, outboxes, outparent, separatorwidth, treewidth
export AbstractAlgebra, apply
export Workspace
export WiringDiagram, DWiringDiagram, FWiringDiagram
export StaticWiringDiagram
export Dendrogram, DDendrogram, FDendrogram
export ArrayAlgebra
export SPDAlgebra

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

include("wiring_diagrams/wiring_diagrams.jl")
include("wiring_diagrams/static_wiring_diagrams.jl")

# ----------- #
# Dendrograms #
# ----------- #

include("dendrograms/dendrograms.jl")

# -------- #
# Algebras #
# -------- #

include("algebras/array_algebras.jl")
include("algebras/spd_algebras.jl")

# ---------- #
# Operations #
# ---------- #

include("operations.jl")

end
