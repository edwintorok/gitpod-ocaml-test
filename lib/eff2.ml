open Effect
open Effect.Deep
open Printf

type _ Effect.t += Conversion_failure : string -> int t

let int_of_string s =
  match int_of_string_opt s with
  | Some n -> n
  | None -> perform (Conversion_failure s)

let sum_stringlist lst = lst |> List.map int_of_string |> List.fold_left ( + ) 0

let safe_sum_stringlist lst =
  try_with sum_stringlist lst
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Conversion_failure s ->
              Some
                (fun (k : (a, _) continuation) ->
                  printf "Bad input: %s, replaced with 0\n" s;
                  continue k 0)
          | _ -> None);
    }

module Shallow = struct
  open Effect.Shallow
  open Printf

  type _ Effect.t += Conversion_failure : string -> int t

  let int_of_string s =
    match int_of_string_opt s with
    | Some n -> n
    | None -> perform (Conversion_failure s)

  let sum_stringlist lst = lst |> List.map int_of_string |> List.fold_left ( + ) 0

  let noeffects = {
    retc = Fun.id;
    exnc = raise;
    effc = fun (type a) (_eff: a Effect.t) -> None
  }

  let safe_sum_stringlist lst =
    continue_with (fiber @@ fun () -> sum_stringlist lst) ()
      { retc = Fun.id
      ; exnc = raise
      ; effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Conversion_failure s ->
                Some
                  (fun (k : (a, _) continuation) ->
                    printf "Bad input: %s, replaced with 0\n" s;
                    continue_with k 0 noeffects)
            | _ -> None);
      }
end
