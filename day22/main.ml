open Base
open Stdio

type state = Clean | Weakened | Infected | Flagged
type dir = U | D | L | R

let move (r, c) = function
  | U -> r-1, c | L -> r, c-1
  | D -> r+1, c | R -> r, c+1

let turn current turn =
  match current, turn with
  | U, `L -> L | U, `R -> R
  | L, `L -> D | R, `R -> D
  | D, `L -> R | D, `R -> L
  | R, `L -> U | L, `R -> U

let reverse = function U -> D | D -> U | L -> R | R -> L

let () =
  let t = In_channel.read_lines "input" in
  let grid =
    List.concat_mapi t ~f:(fun r x ->
      String.to_array x
      |> Array.mapi ~f:(fun c y ->
        let state = if Char.(y = '.') then Clean else Infected in
        (r, c), state)
      |> Array.to_list)
    |> Hashtbl.Poly.of_alist_exn in
  let start = let n = List.length t in n/2, n/2 in
  let a =
    let grid = Hashtbl.copy grid in
    let burst coord dir =
      let state = ref Clean in
      Hashtbl.update grid coord ~f:(function
        | None | Some Clean -> state := Infected; !state
        | Some Infected -> state := Clean; !state
        | Some Weakened | Some Flagged -> assert false);
      !state, turn dir (match !state with
        | Infected -> `L
        | Clean -> `R
        | Weakened | Flagged -> assert false)
    in
    let rec go i coord dir sum =
      if i = 10_000 then sum else
        let state, dir = burst coord dir in
        let sum = match state with
          | Infected -> sum+1
          | Clean | Weakened | Flagged -> sum in
        go (i+1) (move coord dir) dir sum in
    go 0 start U 0 in
  printf "a) %d\n" a;
  let b =
    let grid = Hashtbl.copy grid in
    let burst coord dir =
      let state = ref Clean in
      Hashtbl.update grid coord ~f:(function
        | None | Some Clean -> state := Weakened; !state
        | Some Weakened -> state := Infected; !state
        | Some Infected -> state := Flagged; !state
        | Some Flagged -> state := Clean; !state);
      !state, (match !state with
        | Weakened -> turn dir `L
        | Infected -> dir
        | Flagged -> turn dir `R
        | Clean -> reverse dir)
    in
    let rec go i coord dir sum =
      if i = 10_000_000 then sum else
        let state, dir = burst coord dir in
        let sum = match state with
          | Infected -> sum+1
          | Clean | Weakened | Flagged -> sum in
        go (i+1) (move coord dir) dir sum in
    go 0 start U 0 in
  printf "b) %d\n" b
