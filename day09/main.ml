open Base
open Stdio

type state = Regular | Garbage

let () =
  let t = List.(In_channel.read_lines "input" >>= String.to_list) in
  let rec go prev sum nc state stream =
    match state, stream with
    | _, [] -> sum, nc
    | Regular, '<'::xs -> go prev sum nc Garbage xs
    | Regular, '{'::xs -> go (prev+1) (sum+prev+1) nc Regular xs
    | Regular, '}'::xs -> go (prev-1) sum nc Regular xs
    | Regular, _::xs -> go prev sum nc Regular xs
    | Garbage, '>'::xs -> go prev sum nc Regular xs
    | Garbage, '!'::_::xs -> go prev sum nc Garbage xs
    | Garbage, _::xs -> go prev sum (nc+1) Garbage xs
  in
  let a, b = go 0 0 0 Regular t in
  printf "a) %d\n" a;
  printf "b) %d\n" b
