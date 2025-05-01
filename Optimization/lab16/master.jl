include("common.jl")
using .Common, HTTP, HTTP.WebSockets, JSON3, Dates, Statistics

const ISOLATION_TIME = parse(Int, get(ENV, "ISO_TIME", "30"))
const HOST           = get(ENV, "HOST", "127.0.0.1")
const PORT           = parse(Int, get(ENV, "PORT", "8084"))

populations = Dict{String, Vector{Individual}}()
gen_counts  = Dict{String, Int}()
default_time = rand(0.5:0.001:0.6, 1)

const COUNTER = Ref(0)
next_uid() = (COUNTER[] += 1; "worker" * string(COUNTER[]))

# === История для визуализации ===
history = []

HTTP.WebSockets.listen(HOST, PORT) do ws
    uid = next_uid()
    println(" $uid connected")
    populations[uid] = []
    gen_counts[uid]  = 0

    try
        t_start = time()

        for raw in ws
            data       = String(raw)
            payload    = JSON3.read(data)
            gen        = payload["gen"]
            pop_serial = payload["pop"]
            pop        = [JSON3.read(s, Individual) for s in pop_serial]

            populations[uid] = pop
            gen_counts[uid]  = gen

            # === Добавим в историю ===
            push!(history, Dict(
                "gen" => gen,
                "worker" => uid,
                "points" => [ind.genes for ind in pop]
            ))

            # === Отладочная печать ===
            fits     = [ind.fitness for ind in pop]
            best_val = -maximum(fits)
            avg_val  = -mean(fits)

            println("Generation $gen;  $uid -> best = $(round(best_val, digits=4)), avg = $(round(avg_val, digits=4))")

            # === Обмен мигрантами ===
            migrants = Individual[]
            if gen % ISOLATION_TIME == 0
                for p in values(populations)
                    if !isempty(p)
                        _, idx = findmax([ind.fitness for ind in p])
                        push!(migrants, p[idx])
                    end
                end
            end

            msg_out = JSON3.write(Dict("migrants" => [JSON3.write(m) for m in migrants]))
            HTTP.WebSockets.send(ws, msg_out)
        end

        println("PARALLEL TIME:", time() - t_start)
        println("DEFAULT TIME: ", default_time)

    catch e
        @warn " error   $uid: $e"

    finally
        println("  $uid disconnected")
        delete!(populations, uid)
        delete!(gen_counts, uid)

        # === Сохраняем историю ===
        open("history.json", "w") do f
            JSON3.write(f, history)
        end
        println("[Master] History written to history.json")
    end
end
