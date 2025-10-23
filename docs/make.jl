using CliqueTrees
using Documenter

makedocs(;
    modules = [WiringDiagrams],
    format = Documenter.HTML(),
    sitename = "WiringDiagrams.jl",
    checkdocs = :none,
    pages = ["Library Reference" => "api.md"],
)

deploydocs(;
    target = "build", repo = "github.com/AlgebraicJulia/WiringDiagrams.jl.git", branch = "gh-pages"
)
