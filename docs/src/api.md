# Library Reference

## Operations

```@docs
AbstractOperation
Operation
arity
eachargument
domain
codomain
compose
```

## Wiring Diagrams

```@docs
AbstractWiringDiagram
WiringDiagram
StaticWiringDiagram
nb(::AbstractWiringDiagram)
nw(::AbstractWiringDiagram)
np(::AbstractWiringDiagram)
nop(::AbstractWiringDiagram)
boxes(::AbstractWiringDiagram)
wires(::AbstractWiringDiagram)
wirelabels(::AbstractWiringDiagram)
ports(::AbstractWiringDiagram)
portwires(::AbstractWiringDiagram)
outports(::AbstractWiringDiagram)
outportwires(::AbstractWiringDiagram)
outportlabels(::AbstractWiringDiagram)
np(::AbstractWiringDiagram, ::Integer)
ports(::AbstractWiringDiagram, ::Integer)
portwires(::AbstractWiringDiagram, ::Integer)
portlabels(::AbstractWiringDiagram, ::Integer)
label(::AbstractWiringDiagram, ::Integer)
wire(::AbstractWiringDiagram, ::Integer)
portlabel(::AbstractWiringDiagram, ::Integer)
outwire(::AbstractWiringDiagram, ::Integer)
outportlabel(::AbstractWiringDiagram, ::Integer)
```

## Dendrograms

```@docs
AbstractDendrogram
Dendrogram
nb(::AbstractDendrogram)
nob(::AbstractDendrogram)
nw(::AbstractDendrogram)
np(::AbstractDendrogram)
nop(::AbstractDendrogram)
boxes(::AbstractDendrogram)
outboxes(::AbstractDendrogram)
wires(::AbstractDendrogram)
wirelabels(::AbstractDendrogram)
ports(::AbstractDendrogram)
outports(::AbstractDendrogram)
np(::AbstractDendrogram, ::Integer)
ports(::AbstractDendrogram, ::Integer)
portwires(::AbstractDendrogram, ::Integer)
portlabels(::AbstractDendrogram, ::Integer)
AbstractTrees.parent(::AbstractDendrogram, ::Integer)
nop(::AbstractDendrogram, ::Integer)
outports(::AbstractDendrogram, ::Integer)
outportwires(::AbstractDendrogram, ::Integer)
outportlabels(::AbstractDendrogram, ::Integer)
outparent(::AbstractDendrogram, ::Integer)
nb(::AbstractDendrogram, ::Integer)
boxes(::AbstractDendrogram, ::Integer)
nob(::AbstractDendrogram, ::Integer)
outboxes(::AbstractDendrogram, ::Integer)
nw(::AbstractDendrogram, ::Integer)
wires(::AbstractDendrogram, ::Integer)
wirelabels(::AbstractDendrogram, ::Integer)
label(::AbstractDendrogram, ::Integer)
wire(::AbstractDendrogram, ::Integer)
portlabel(::AbstractDendrogram, ::Integer)
outwire(::AbstractDendrogram, ::Integer)
outportlabel(::AbstractDendrogram, ::Integer)
treewidth
separatorwidth
```

## Algebras

```@docs
AbstractAlgebra
apply
ArrayAlgebra                                                                                                 
```
