open Base
open Stdio

let char2int c = Char.(to_int c - to_int '0')

let parse file =
  let s = In_channel.read_lines file |> List.hd_exn in
  String.length s
  |> Array.init ~f:(fun i -> char2int s.[i])

let solve xs offset =
  let open Array in
  let n = length xs in
  xs
  |> filteri ~f:(fun i x -> x = xs.(Caml.(mod) (offset+i) n))
  |> fold ~init:0 ~f:(+)

let () =
  let t = parse "input" in
  let half = Array.length t / 2 in
  printf "a) %d\nb) %d\n" (solve t 1) (solve t half)
