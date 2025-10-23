#=
using WiringDiagrams
using Documenter

makedocs(;
    modules = [WiringDiagrams],
    format = Documenter.HTML(),
    sitename = "WiringDiagrams.jl",
    checkdocs = :none,
    pages = ["WiringDiagrams.jl" => "index.md", "Library Reference" => "api.md"],
)

deploydocs(;
    target = "build", repo = "github.com/AlgebraicJulia/WiringDiagrams.jl.git", branch = "gh-pages"
)
=#
