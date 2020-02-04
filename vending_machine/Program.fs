// Original Vending machine

open System

type State = | S_0
             | S_5
             | S_10
             | S_15
             | S_ERROR

type Transition = | T_5kr
                  | T_10kr
                  | T_tea
                  | T_coffee

let program = [T_5kr; T_10kr; T_coffee]

let rec machine c_s = function
    | []    -> c_s
    | t::ts -> match c_s with
                   | S_0     -> match t with
                                | T_5kr  -> machine S_5 ts
                                | T_10kr -> machine S_10 ts
                                | _      -> machine S_ERROR ts
                   | S_5     -> match t with
                                | T_5kr  -> machine S_10 ts
                                | T_10kr -> machine S_15 ts
                                | _      -> machine S_ERROR ts
                   | S_10    -> match t with
                                | T_5kr    -> machine S_15 ts
                                | T_10kr   -> machine S_15 ts
                                | T_tea    -> machine S_0 ts
                                | T_coffee -> machine S_ERROR ts
                   | S_15    -> match t with
                                | T_coffee -> machine S_0 ts
                                | _        -> machine S_ERROR ts
                   | S_ERROR -> machine S_ERROR ts

[<EntryPoint>]
let main argv =
    printfn "%A" (machine S_0 program)
    0 // return an integer exit code
