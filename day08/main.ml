open Base
open Stdio

type reg = string
type instr = { reg : reg; op : int -> int; cond : (int -> bool) * reg }

let parse s =
  let open Caml.Scanf in
  sscanf s "%s %s %d if %s %s %d" (fun reg op magn reg2 cond magn2 ->
    let cond = match cond with
      | "==" -> fun x -> x = magn2
      | "!=" -> fun x -> x <> magn2
      | "<" -> fun x -> x < magn2
      | ">" -> fun x -> x > magn2
      | "<=" -> fun x -> x <= magn2
      | ">=" -> fun x -> x >= magn2
      | _ -> assert false in
    let op = match op with
      | "inc" -> fun x -> x + magn
      | "dec" -> fun x -> x - magn
      | _ -> assert false in
    { reg; op; cond = cond, reg2 })

let () =
  let open List in
  let instrs = In_channel.read_lines "input" >>| parse in
  let init = Map.empty (module String), 0 in
  let max_value m = Map.to_alist m >>| snd |> reduce_exn ~f:max in
  let end_state, max_reg = fold instrs ~init ~f:(fun (state, m) instr ->
    let { reg; op; cond = cond, reg2 } = instr in
    let find r = Map.find state r |> Option.value ~default:0 in
    if cond (find reg2) then
      let state = Map.update state reg ~f:(function
        | None -> op 0
        | Some x -> op x) in
      state, max m (max_value state)
    else state, m)
  in
  printf "a) %d\n" (max_value end_state);
  printf "b) %d\n" max_reg
