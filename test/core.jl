using Graphs
using OMEinsum
using WiringDiagrams

function random_regular_eincode()
    graph = random_regular_graph(10, 3)

    array = Array{Float64}[]
    inputs = Vector{Tuple{Int, Int}}[]
    output = Tuple{Int, Int}[]
    size_dict = Dict{Tuple{Int, Int}, Int}()

    for v in vertices(graph)
        input = Tuple{Int, Int}[]; dim = Int[]
        
        for w in neighbors(graph, v)
            e = minmax(v, w)

            if !haskey(size_dict, e)
                size_dict[e] = rand(2:5)
            end
            
            push!(input, e); push!(dim, size_dict[e])
            
            if rand() < 0.1
                push!(output, e)
            end
        end

        push!(inputs, input)
        push!(array, 0.5 * rand(dim...))
    end

    return array, inputs, output, size_dict
end

@testset "random tensor networks" begin
    algebra = ArrayAlgebra{Array{Float64}}()

    for _ in 1:10
        array, inputs, output, size_dict = random_regular_eincode()

        eincode = DynamicEinCode(inputs, output)
        diagram = WiringDiagram(inputs, output, size_dict)
        dendrogram = Dendrogram(log2, diagram)

        out1 = einsum(eincode, Tuple(array))
        out2 = algebra(diagram)(array...)
        out3 = algebra(dendrogram)(array...)

        @test out1 ≈ out2
        @test out2 ≈ out3
        @test out1 ≈ out3
    end
end
