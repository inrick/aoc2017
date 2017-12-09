open Base
open Stdio

type state = Regular | Garbage

let () =
  let t = List.(In_channel.read_lines "input" >>= String.to_list) in
  let rec go curr sum nc state stream =
    match state, stream with
    | _, [] -> sum, nc
    | Regular, '<'::xs -> go curr sum nc Garbage xs
    | Regular, '{'::xs -> go (curr+1) (sum+curr) nc Regular xs
    | Regular, '}'::xs -> go (curr-1) sum nc Regular xs
    | Regular, _::xs -> go curr sum nc Regular xs
    | Garbage, '>'::xs -> go curr sum nc Regular xs
    | Garbage, '!'::_::xs -> go curr sum nc Garbage xs
    | Garbage, _::xs -> go curr sum (nc+1) Garbage xs
  in
  let a, b = go 1 0 0 Regular t in
  printf "a) %d\n" a;
  printf "b) %d\n" b
