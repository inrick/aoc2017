open Base
open Stdio

type reg = char
type value = Imm of int | Reg of reg
type instr =
  | Set of reg * value
  | Add of reg * value
  | Mul of reg * value
  | Mod of reg * value
  | Jgz of value * value
  | Snd of value
  | Rcv of reg

type state = Waiting | Done

type t =
  { regs : char array; vals : int array; pc : int; snd : int; state : state }

let parse input =
  let parse_reg = Char.of_string in
  let parse_value x = try Imm (Int.of_string x) with _ -> Reg (parse_reg x) in
  let parse_instr s =
    let xs = String.split s ~on:' ' |> Array.of_list in
    match xs.(0) with
    | "set" -> Set (parse_reg xs.(1), parse_value xs.(2))
    | "add" -> Add (parse_reg xs.(1), parse_value xs.(2))
    | "mul" -> Mul (parse_reg xs.(1), parse_value xs.(2))
    | "mod" -> Mod (parse_reg xs.(1), parse_value xs.(2))
    | "jgz" -> Jgz (parse_value xs.(1), parse_value xs.(2))
    | "snd" -> Snd (parse_value xs.(1))
    | "rcv" -> Rcv (parse_reg xs.(1))
    | _ -> assert false in
  let instrs = List.map input ~f:parse_instr in
  let regs = (* maybe sort this out better? *)
    List.filter_map instrs ~f:(function
      | Set (r, _) | Add (r, _) | Mul (r, _) | Mod (r, _)
      | Snd (Reg r) | Rcv r -> Some r
      | Snd _ | Jgz (_, _) -> None)
    |> List.dedup ~compare:Char.compare in
  Array.(of_list regs, of_list instrs)

let create regs =
  let vals = Array.(create ~len:(length regs) 0) in
  { regs; vals; pc=0; snd=0; state=Waiting }

let findi regs r =
  Array.findi_exn regs (fun _ -> Char.(=) r) |> fst

let inc_pc st = { st with pc = st.pc + 1 }

let get st r = st.vals.(findi st.regs r)

let seti st i x =
  let vals = Array.copy st.vals in
  vals.(i) <- x;
  { st with vals = vals }

let set st r x = seti st (findi st.regs r) x

let value_of st = function Reg r -> get st r | Imm i -> i

let amend st op r x =
  let i = findi st.regs r in
  seti st i (op st.vals.(i) (value_of st x))

let jgz st x y =
  let x, y = value_of st x, value_of st y in
  { st with pc = st.pc + (if 0 < x then y else 1) }

let execute regs instrs =
  let rec eval st = function
    | Set (r, x) -> set st r (value_of st x) |> inc_pc |> go
    | Add (r, x) -> amend st (+) r x |> inc_pc |> go
    | Mul (r, x) -> amend st ( * ) r x |> inc_pc |> go
    | Mod (r, x) -> amend st (%) r x |> inc_pc |> go
    | Jgz (x, y) -> jgz st x y |> go
    | Snd x -> { st with snd = value_of st x } |> inc_pc |> go
    | Rcv x -> if get st x <> 0 then st.snd else inc_pc st |> go
  and go st = eval st instrs.(st.pc) in
  go (create regs)

let execute2 regs instrs =
  let module Q = Linked_queue in
  let rec eval st instr qsnd qrcv =
    match instr with
    | Set (r, x) -> go (set st r (value_of st x) |> inc_pc) qsnd qrcv
    | Add (r, x) -> go (amend st (+) r x |> inc_pc) qsnd qrcv
    | Mul (r, x) -> go (amend st ( * ) r x |> inc_pc) qsnd qrcv
    | Mod (r, x) -> go (amend st (%) r x |> inc_pc) qsnd qrcv
    | Jgz (x, y) -> go (jgz st x y) qsnd qrcv
    | Snd x ->
      Q.enqueue qsnd (value_of st x);
      go (inc_pc { st with snd=st.snd+1 }) qsnd qrcv
    | Rcv x ->
      begin match Q.dequeue qrcv with
      | None -> { st with state=Waiting }
      | Some y -> go (set st x y |> inc_pc) qsnd qrcv
      end
  and go st qsnd qrcv =
    match instrs.(st.pc) with
    | instr -> eval st instr qsnd qrcv
    | exception _ -> { st with state=Done }
  in
  let q1, q2 = Q.create (), Q.create () in
  let rec handler st0 st1 =
    let st0 = go st0 q1 q2 in
    let st1 = go st1 q2 q1 in
    match st0.state, st1.state with
    | Done, Done -> st0, st1
    | Done, Waiting when Q.is_empty q1 -> st0, st1
    | Waiting, Done when Q.is_empty q2 -> st0, st1
    | Waiting, Waiting when Q.is_empty q1 && Q.is_empty q2 -> st0, st1
    | Done, Waiting | Waiting, Done | Waiting, Waiting -> handler st0 st1
  in
  let st0 = create regs in
  st0.vals.(findi st0.regs 'p') <- 0;
  let st1 = create regs in
  st1.vals.(findi st1.regs 'p') <- 1;
  let st0, st1 = handler st0 st1 in
  st1.snd

let () =
  let regs, instrs = parse (In_channel.read_lines "input") in
  printf "a) %d\n" (execute regs instrs);
  printf "b) %d\n" (execute2 regs instrs)
