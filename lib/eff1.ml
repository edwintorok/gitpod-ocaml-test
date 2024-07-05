open Effect
open Effect.Deep
open Printf

type _ eff += Conversion_failure : string -> int eff

let int_of_string s =
  match int_of_string_opt s with
  | Some n -> n
  | None -> perform (Conversion_failure s)

let sum_stringlist lst =
  lst |> List.map int_of_string |> List.fold_left (+) 0

let safe_sum_stringlist lst =
  match sum_stringlist lst with
  | res -> res
  | effect Conversion_failure s, k ->
    printf "Bad input: %s, replaced with 0\n" s;
    continue k 0
