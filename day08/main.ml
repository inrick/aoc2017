open Base
open Stdio

type reg = string
type op = int -> int -> int
type cond = int -> int -> bool
type instr = { reg : reg; op : op * int; cond : cond * reg * int }

let parse s =
  let open Caml.Scanf in
  sscanf s "%s %s %d if %s %s %d" (fun reg op magn reg2 cond magn2 ->
    let cond = match cond with
      | "==" -> (=)
      | "!=" -> (<>)
      | "<" -> (<)
      | ">" -> (>)
      | "<=" -> (<=)
      | ">=" -> (>=)
      | _ -> assert false in
    let op = match op with
      | "inc" -> (+)
      | "dec" -> (-)
      | _ -> assert false in
    { reg; op = op, magn; cond = cond, reg2, magn2 })

let exec instrs =
  let regs = List.(instrs
    >>| (fun i -> i.reg)
    |> dedup ~compare:String.compare
    |> to_array) in
  let find_reg s = Array.findi_exn regs ~f:(fun _ -> String.(=) s) |> fst in
  let array_max xs = Array.max_elt xs ~cmp:Int.compare |> Option.value_exn in
  let n = Array.length regs in
  let c = Array.create ~len:n 0 in
  let maxreg = ref 0 in
  let step { reg; op = op, magn; cond = cond, reg2, magn2 } =
    let m' = array_max c in
    if m' > !maxreg then maxreg := m';
    let i = find_reg reg in
    let reg2 = c.(find_reg reg2) in
    if cond reg2 magn2 then c.(i) <- op c.(i) magn
  in
  List.iter instrs ~f:step;
  array_max c, !maxreg

let () =
  let open List in
  let instrs = In_channel.read_lines "input" |> List.map ~f:parse in
  let a, b = exec instrs in
  printf "a) %d\n" a;
  printf "b) %d\n" b
