open Base
open Stdio

type reg = char
type value = Imm of int | Reg of reg
type instr =
  | Set of reg * value
  | Sub of reg * value
  | Mul of reg * value
  | Jnz of value * value

type t =
  { regs : char array; vals : int array; pc : int; mul : int }

let parse input =
  let parse_reg = Char.of_string in
  let parse_value x = try Imm (Int.of_string x) with _ -> Reg (parse_reg x) in
  let parse_instr s =
    let xs = String.split s ~on:' ' |> Array.of_list in
    match xs.(0) with
    | "set" -> Set (parse_reg xs.(1), parse_value xs.(2))
    | "sub" -> Sub (parse_reg xs.(1), parse_value xs.(2))
    | "mul" -> Mul (parse_reg xs.(1), parse_value xs.(2))
    | "jnz" -> Jnz (parse_value xs.(1), parse_value xs.(2))
    | _ -> assert false in
  let instrs = List.map input ~f:parse_instr in
  let regs = String.to_array "abcdefgh" in
  regs, Array.of_list instrs

let create regs =
  let vals = Array.(create ~len:(length regs) 0) in
  { regs; vals; pc=0; mul=0 }

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

let jnz st x y =
  let x, y = value_of st x, value_of st y in
  { st with pc = st.pc + (if 0 <> x then y else 1) }

let inc_mul st = { st with mul = st.mul + 1 }

let execute init instrs =
  let rec eval st = function
    | Set (r, x) -> set st r (value_of st x) |> inc_pc |> go
    | Sub (r, x) -> amend st (-) r x |> inc_pc |> go
    | Mul (r, x) -> amend st ( * ) r x |> inc_pc |> inc_mul |> go
    | Jnz (x, y) -> jnz st x y |> go
  and go st = match instrs.(st.pc) with
    | i -> eval st i
    | exception _ -> st.mul in
  go init

let () =
  let regs, instrs = parse (In_channel.read_lines "input") in
  let a = execute (create regs) instrs in
  printf "a) %d\n" a
