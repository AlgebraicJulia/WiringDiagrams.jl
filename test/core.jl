using Graphs
using OMEinsum
using WiringDiagrams

@testset "Hidden Markov Model" begin
    #
    #   ┌───┐  ┌───┐  ┌───┐
    # ──┤ A ├──┤ B ├──┤ C ├──
    #   └─┬─┘  └─┬─┘  └─┬─┘
    #   ┌─┴─┐  ┌─┴─┐  ┌─┴─┐
    #   │ D │  │ E │  │ F │
    #   └───┘  └───┘  └───┘
    #
    inputs = [[1, 2, 3], [3, 4, 5], [5, 6, 7], [2], [4], [6]]
    output = [1, 7]

    @testset "Printing" begin
        label = Dict(1 => 3, 2 => 3, 3 => 3, 4 => 3, 5 => 3, 6 => 3, 7 => 3)
        diagram = WiringDiagram(inputs, output, label)
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
        A = ones(3, 3, 3)
        B = ones(3, 3, 3)
        C = ones(3, 3, 3)

        D = ones(3)
        E = ones(3)
        F = ones(3)

        label = Dict(1 => 3, 2 => 3, 3 => 3, 4 => 3, 5 => 3, 6 => 3, 7 => 3)
        diagram = WiringDiagram(inputs, output, label)
        dendrogram = Dendrogram(log2, diagram)

        algebra = ArrayAlgebra{Array{Float64}}()

        @test algebra(diagram)(A, B, C, D, E, F) == 243ones(3, 3)
        @test algebra(dendrogram)(A, B, C, D, E, F) == 243ones(3, 3)
    end

    @testset "SpanAlgebra" begin
        labels = Dict(1 => 3, 2 => 3, 3 => 3, 4 => 3, 5 => 3, 6 => 3, 7 => 3)

        A = [2 2 1; 1 1 2; 3 3 1]
        B = [1 2 1; 2 2 1; 1 1 2]
        C = [1 3 2; 1 2 1; 2 1 1]

        D = [1 2 3]
        E = [2 1 3]
        F = [3 3 1]

        algebra = SpanAlgebra()

        label = Dict(1 => 3, 2 => 3, 3 => 3, 4 => 3, 5 => 3, 6 => 3, 7 => 3)
        diagram = WiringDiagram(inputs, output, label)
        dendrogram = Dendrogram(log2, diagram)

        @test algebra(diagram)(A, B, C, D, E, F) == [1 1; 2 1]
        @test algebra(dendrogram)(A, B, C, D, E, F) == [1 1; 2 1]
    end
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
