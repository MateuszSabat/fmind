module genetics

let private rng = System.Random()
let private random min max = rng.Next (min, max)

type competition_result = FirstWon | SecondWon | Draw

let iterate crossover (mutation: 'a -> 'a) competition competition_levels generation = 
    let length = Array.length generation

    let individul index = Array.get generation index

    let competition first second = 
        match competition (individul first) (individul second) with 
        | FirstWon -> (first, second) 
        | SecondWon -> (second, first)
        | Draw -> (first, second)

    let rec choose_best_worst levels = 
        if levels = 1 then 
            competition (random 0 length) (random 0 length)
        else 
            let (best0, worst0) = choose_best_worst (levels-1)
            let (best1, worst1) = choose_best_worst (levels-1)

            let (best, _) = competition best0 best1
            let (_, worst) = competition worst0 worst1
            
            (best, worst)

    let (best0, worst0) = choose_best_worst competition_levels
    let (best1, worst1) = choose_best_worst competition_levels

    let (child0, child1) = crossover (individul best0) (individul best1)

    Array.set generation worst0 (mutation child0)
    Array.set generation worst1 (mutation child1)

let evolve (evolution: unit -> 'a -> 'a) generation =
    let evolution = evolution ()
    List.map evolution generation


    