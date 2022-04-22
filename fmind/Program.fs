open neural_tree
open Thoth.Json.Net

[<EntryPoint>]
let main argv = 

    let rnd = System.Random()

    let random_offset magnitude = (rnd.NextDouble () - 1.0) * if rnd.Next() % 2 = 0 then -magnitude else magnitude
    let mutation x = x + random_offset 0.1

    let inputs = 4
    let outputs = 3

    let full_mutation =
        let rec rec_mutation index p = if index > inputs then p else rec_mutation (index+1) (perceptron.mutate mutation index p)
        rec_mutation 0 
        
    let random_network inputs outputs = List.map full_mutation (network.zero inputs outputs)

    let net = 
        random_network inputs outputs
        |> network.complexify_at 0
        |> network.complexify_at 1
        |> network.expand_at 1
    
    let fire = network.fire net
    let rec print_list = function
        | [] -> printfn "%s" ""
        | h::t -> printf " %f " h; print_list t

    print_list (fire [1.0;2.0;3.0;4.0;5.0])
    
    printfn "%s" (Encode.Auto.toString (2, net))

    0