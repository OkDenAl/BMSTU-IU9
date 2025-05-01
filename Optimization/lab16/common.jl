module Common

export Individual, evaluate!, crossover!, mutate!, select_parents!

mutable struct Individual
    genes::Vector{Float64}
    fitness::Float64
end


function evaluate!(ind::Individual)
    val = 0.0
    for i in 1:length(ind.genes)-1
        xi, xj = ind.genes[i], ind.genes[i+1]
        val += 100 * (xj - xi^2)^2 + (1 - xi)^2
    end
    ind.fitness = -val
end

function crossover!(p1::Individual, p2::Individual)
    pt = rand(1:length(p1.genes))
    c1 = Individual(vcat(p1.genes[1:pt], p2.genes[pt+1:end]), 0.0)
    c2 = Individual(vcat(p2.genes[1:pt], p1.genes[pt+1:end]), 0.0)
    return c1, c2
end

function mutate!(ind::Individual; σ=0.1, p_mut=0.1)
    for i in eachindex(ind.genes)
        if rand() < p_mut
            ind.genes[i] += σ * randn()
        end
    end
end

function select_parents!(pop::Vector{Individual}; k=3)

    return sort(pop, by = x -> x.fitness, rev = true)[1:k]
end

end 
