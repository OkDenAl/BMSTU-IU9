include("common.jl")
using .Common, HTTP, HTTP.WebSockets, JSON3, Random

# ——————————————————————————————————————————————
const HOST             = get(ENV, "HOST", "127.0.0.1")
const PORT             = get(ENV, "PORT", "8084")
const SERVER_URL       = "ws://$HOST:$PORT"
const POPULATION_SIZE  = parse(Int, get(ENV, "POP_SIZE", "100"))
const MAX_GENERATIONS  = 200
const CROSSOVER_PROB   = 0.9
const MUTATION_PROB    = 0.1
const GENE_COUNT       = 2
const STOP_FITNESS     = 1e-4
const N                = 3
# ——————————————————————————————————————————————

function init_population(n::Int, gene_len::Int)
    pop = [Individual(rand(-5.0:0.2:5.0, gene_len), 0.0) for _ in 1:n]
    for ind in pop
        evaluate!(ind)
    end
    return pop
end

function breed_next(pop::Vector{Individual})
    gene_len = length(pop[1].genes)
    D = gene_len

    maxi = [ maximum(ind.genes[j] for ind in pop) for j in 1:gene_len ]
    mini = [ minimum(ind.genes[j] for ind in pop) for j in 1:gene_len ]

    children = Individual[]
    while length(children) < POPULATION_SIZE
        mom, dad = select_parents!(pop)

        if rand() < CROSSOVER_PROB
            c1, c2 = crossover!(mom, dad)
        else
            c1, c2 = deepcopy(mom), deepcopy(dad)
        end

        norm_diff_sum = 0.0
        for i in 1:gene_len
            denom = maxi[i] - mini[i]
            if denom != 0.0
                norm = abs((mom.genes[i] - dad.genes[i]) / denom)
                norm_diff_sum += norm^D
            end
        end

        dist = norm_diff_sum / N
        mutate!(c1, p_mut = 1 - dist)
        mutate!(c2, p_mut = 1 - dist)
        evaluate!(c1)
        evaluate!(c2)
        push!(children, c1, c2)
    end

    return children[1:POPULATION_SIZE]
end

function send_state(ws, generation::Int, pop::Vector{Individual})
    payload = Dict(
        "gen" => generation,
        "pop" => [JSON3.write(ind) for ind in pop]
    )
    HTTP.WebSockets.send(ws, JSON3.write(payload))
end

function receive_migrants(ws)
    raw = HTTP.WebSockets.receive(ws)
    doc = JSON3.read(String(raw))
    migrants = [JSON3.read(String(s), Individual) for s in doc["migrants"]]
    return migrants
end

function integrate_migrants(pop::Vector{Individual}, m::Vector{Individual})
    if isempty(m)
        return pop
    end
    sorted = sort(pop, by = x->x.fitness)  
    for (i, mig) in enumerate(m)
        sorted[i] = mig
    end
    return sorted
end

function evolve!(ws)
    population = init_population(POPULATION_SIZE, GENE_COUNT)
    for gen in 1:MAX_GENERATIONS
        population = breed_next(population)
        send_state(ws, gen, population)

        migrants = receive_migrants(ws)
        population = integrate_migrants(population, migrants)

        best_val = -maximum(ind->ind.fitness, population)
        println("Best generation $gen -> $(round(best_val, digits=4))")
        if best_val < STOP_FITNESS
            println("Last generation $gen: fitness < $STOP_FITNESS")
            break
        end
    end

    final_best = -maximum(ind->ind.fitness, population)
    println("Best fitness -> $(round(final_best, digits=4))")
end

function main()
    println("[Worker] connecting $SERVER_URL …")
    HTTP.WebSockets.open(SERVER_URL) do ws
        println("[Worker] connected")
        evolve!(ws)
    end
end

main()
