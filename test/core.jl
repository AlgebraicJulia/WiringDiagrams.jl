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

@testset "Printing" begin
    _, inputs, output, size_dict = random_regular_eincode()
    diagram = WiringDiagram(inputs, output, size_dict)
    static = StaticWiringDiagram(diagram)
    dendrogram = Dendrogram(log2, diagram)
    operation = ArrayAlgebra{Array{Float64}}()(dendrogram)

    @test isa(repr("text/plain", diagram), String)
    @test isa(repr("text/plain", static), String)
    @test isa(repr("text/plain", dendrogram), String)
    @test isa(repr("text/plain", operation), String)

    @test isa(repr(diagram), String)
    @test isa(repr(static), String)
    @test isa(repr(dendrogram), String)
    @test isa(repr(operation), String)
end

@testset "ArrayAlgebra" begin
    algebra = ArrayAlgebra{Array{Float64}}()

    for _ in 1:10
        array, inputs, output, size_dict = random_regular_eincode()

        eincode = DynamicEinCode(inputs, output)
        diagram = WiringDiagram(inputs, output, size_dict)
        static = StaticWiringDiagram(diagram)
        dendrogram = Dendrogram(log2, diagram)

        @test 1.0 <= separatorwidth(log2, dendrogram) <= treewidth(log2, dendrogram) <= sum(log2, values(size_dict))

        out1 = einsum(eincode, Tuple(array))
        out2 = algebra(diagram)(array...)
        out3 = algebra(static)(array...)
        out4 = algebra(dendrogram)(array...)

        @test out1 ≈ out2
        @test out2 ≈ out3
        @test out3 ≈ out4
    end
end

@testset "SPDAlgebra" begin
    A = rand(10, 10); A *= A'
    B = rand(10, 10); B *= B'

    A11 = A[1:5,  1:5]; A21 = A[6:10, 1:5]; A22 = A[6:10, 6:10]
    B11 = B[1:5,  1:5]; B21 = B[6:10, 1:5]; B22 = B[6:10, 6:10]

    diagram = WiringDiagram(
        [[0, 1, 2, 3, 4, 0, 1, 2, 3, 4], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]],
        [4, 3, 2, 1, 0],
    )

    algebra = SPDAlgebra{Matrix{Float64}}()

    M = algebra(diagram)(A, B)

    N = [
        A11 + A21 + A21' + A22 + B11 B21'
        B21                          B22
    ]

    @test size(M) == (5, 5)
    @test inv(M) ≈ inv(N)[5:-1:1, 5:-1:1]
end

@testset "AbstractWiringDiagram" begin
    @testset "6.46" begin
        # Seven Sketches in Compositionality
        # Fong and Spivak
        # Example 6.46
        for Dgm in (WiringDiagram, StaticWiringDiagram)
            AB = Dgm(
                [[1, 1, 3, 3, 5]],
                [1, 1, 2, 4, 5, 5],
            )

            BC = Dgm(
                [[1, 1, 3, 4, 4, 5]],
                [1, 2, 4, 4, 5],
            )

            AC = compose(1, BC, AB)

            @test nb(AC) == 1
            @test nw(AC) == 5
            @test np(AC) == 5
            @test nop(AC) == 5

            @test np(AC, 1) == 5

            @test wire(AC, 1) == wire(AC, 2) == outwire(AC, 1)
            @test wire(AC, 3) == wire(AC, 4)
            @test wire(AC, 5) == outwire(AC, 3) == outwire(AC, 4) == outwire(AC, 5)

            @test allunique((wire(AC, 1), wire(AC, 3), wire(AC, 5)))
        end
    end

    @testset "6.96" begin
        # Seven Sketches in Compositionality
        # Fong and Spivak
        # Exercise 6.96
        for Dgm in (WiringDiagram, StaticWiringDiagram)
            AB = Dgm(
                [[1, 2], [2, 3]],
                [1, 3],
            )

            BC = Dgm(
                [[2, 1], [1, 3], [3, 2]],
                [],
            )

            for index in (1, 2, 3)
                AC = compose(index, BC, AB)

                @test nb(AC) == 4
                @test nw(AC) == 4
                @test np(AC) == 8
                @test nop(AC) == 0

                @test np(AC, 1) == np(AC, 2) == np(AC, 3) == np(AC, 4) == 2

                @test wire(AC, 2) == wire(AC, 3)
                @test wire(AC, 4) == wire(AC, 5)
                @test wire(AC, 6) == wire(AC, 7)
                @test wire(AC, 8) == wire(AC, 1)

                @test allunique((wire(AC, 2), wire(AC, 4), wire(AC, 6), wire(AC, 8)))
            end
        end
    end
end
