module neural_tree

open System

type perceptron =
    | Simple of float * (float list)
    | Complex of perceptron list * perceptron

type network = perceptron list

module private utils =        

    module list = 

        let replace index value list =
            let rec replace i tail = 
                match tail with
                | [] -> []
                | h::t -> (if i = index then value else h) :: (replace (i+1) t)
            replace 0 list

        let zip zipper lhs rhs =
            let rec rec_zip = function
                | (h0::t0, h1::t1) -> (zipper h0 h1) :: (rec_zip (t0, t1))
                | _ -> []
            rec_zip (lhs, rhs)

        let zip_safe zipper left_zero right_zero lhs rhs =
            let rec rec_zip = function
                | ([], []) -> []
                | ([], h::t) -> (zipper left_zero h) :: (rec_zip ([], t))
                | (h::t, []) -> (zipper h right_zero) :: (rec_zip (t, []))
                | (h0::t0, h1::t1) -> (zipper h0 h1) :: (rec_zip (t0, t1))
            rec_zip (lhs, rhs)

        let dot lhs rhs : float = List.sum (zip (*) lhs rhs)

    let map_range (a0:float) b0 a1 b1 = 
        let a = (b1 - a1) / (b0 - a0)
        let b = a1 - a0 * a
        fun x -> a * x + b

module perceptron =

    let private from_simple f = function
        | Simple (bias, weights) -> f bias weights
        | Complex _ as c -> c

    let private from_complex f = function
        | Simple _ as s -> s
        | Complex (hiddens, last) -> f hiddens last

    let rec inputs = function
        | Simple (_, weights) -> List.length weights
        | Complex (hiddens, out) -> inputs (List.head hiddens)

    let rec with_new_input = function
        | Simple (bias, weights) -> Simple (bias, 0.0::weights)
        | Complex (hiddens, out) -> Complex (List.map with_new_input hiddens, out)

    module public simple =
        let make bias weights = Simple (bias, weights)

        let create_with bias weight inputs = Simple (bias, List.init inputs (fun _ -> weight()))
            
        let one = create_with 1.0 (fun () -> 1.0)
        let zero = create_with 0.0 (fun () -> 0.0)
        let random rng = create_with (rng()) rng

        let with_bias bias = from_simple (fun _ weights -> Simple (bias, weights))
        let with_weight index weight = from_simple (fun bias weights -> Simple (bias, utils.list.replace index weight weights))

        let internal with_new_weight weight = from_simple (fun bias weights -> Simple (bias, weight::weights))

        let rec get getter folder = function
            | Simple (bias, weights) -> getter bias weights
            | Complex (hidden, out) -> List.fold folder (get getter folder out) (List.map (get getter folder) hidden)
            
        let count = get (fun _ _ -> 1) (+)
        let weight_count = get (fun _ weights -> 1 + List.length weights) (+)

    module public complex =
        let make hiddens out = Complex (hiddens, out)

        let create_with perceptron inputs hidden_count = Complex (List.init hidden_count (fun _ -> perceptron inputs), perceptron hidden_count)
        
        let one = create_with simple.one
        let zero = create_with simple.zero
        let ranadom rng = create_with (simple.random rng)

        let like_simple precision bias weights =
            let inputs = List.length weights
            let identity index = Simple (precision - 2.0, List.init inputs (fun i -> if i = index then 4.0 - 8.0 * precision else 0.0))
            let weights = List.map (fun w -> w / (1.0 - 2.0 * precision)) weights
            Complex (List.init inputs (fun i -> identity i), Simple(bias - (float inputs) * precision / (1.0 - 2.0 * precision), weights))

        let with_hidden index perceptron = from_complex (fun hiddens out -> Complex (utils.list.replace index perceptron hiddens, out))
        let with_out perceptron = from_complex (fun hiddens _ -> Complex (hiddens, perceptron))

        let internal with_new_hidden hiddens out = Complex((simple.zero (inputs (List.head hiddens)))::hiddens, with_new_input out)

        let rec count = function
            | Simple _ -> 0
            | Complex (hiddens, out) -> 1 + List.sum (List.map count (out::hiddens))

        let rec weight_count = function
            | Simple (_, weights) -> 1 + (List.length weights)
            | Complex (hiddens, out) -> List.sum (List.map weight_count (out::hiddens))

    let mutate mutation skip perceptron =
        let mutable index = -1

        let try_mutate parameter = 
            index <- index + 1 
            if index = skip then mutation parameter else parameter

        let rec try_mutate_list parameters = 
            if index > skip then parameters
            else match parameters with
                 | [] -> []
                 | h::t -> (try_mutate h) :: (try_mutate_list t)

        let rec rec_mutate = function
            | Simple (bias, weights) as simple ->
                if index > skip then simple
                else Simple (try_mutate bias, try_mutate_list weights)
            | Complex (hiddens, out) as complex ->
                if index > skip then complex
                else Complex (List.map rec_mutate hiddens, rec_mutate out)

        rec_mutate perceptron

    let private sigmoid x = 1.0 / (1.0 + (Math.Exp (-x)))

    let rec fire inputs = function
        | Simple (bias, weights) -> sigmoid (bias + (utils.list.dot inputs weights))
        | Complex (hiddens, out) -> fire (List.map (fire inputs) hiddens) out

module network =

    let zero inputs outputs : network = List.init outputs (fun _ -> perceptron.simple.zero inputs)

    module simple =
        let count network =  List.sum (List.map perceptron.simple.count network)

        let replace_at replacement index (network:network) : network =
            let mutable i = -1

            let rec replace_simple = function
                | Simple (bias, weights) as simple -> 
                    i <- (i+1)
                    if i = index then replacement bias weights else simple
                | Complex (hiddens, out) as complex ->
                    if i > index then complex else Complex (List.map replace_simple hiddens, replace_simple out)

            List.map replace_simple network

    module complex =
        let count network = List.sum (List.map perceptron.complex.count network)

        let replace_at replacement index (network:network) : network =
            let mutable i = -1

            let rec replace_complex = function
                | Simple _ as simple -> simple
                | Complex (hiddens, out) as complex ->
                    i <- (i+1)
                    if i > index then complex
                    else if i = index then replacement hiddens out
                    else Complex (List.map replace_complex hiddens, replace_complex out)

            List.map replace_complex network

    let complexify_at = simple.replace_at (perceptron.complex.like_simple 0.3)
    let expand_at = complex.replace_at perceptron.complex.with_new_hidden

    let fire network inputs = List.map (perceptron.fire inputs) network