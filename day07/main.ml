open Base
open Stdio

type t = T of string * int * t list [@@deriving sexp]

let weight (T (_, w, _)) = w

let show x = sexp_of_t x |> Sexp.to_string_hum

let parse s =
  let open Caml.Scanf in
  let split s = List.(String.split s ~on:',' >>| String.strip) in
  try sscanf s "%s (%d) -> %s@\n" (fun n w xs -> n, w, split xs)
  with _ -> sscanf s "%s (%d)" (fun n w -> n, w, [])

let () =
  let open List in
  let ts = In_channel.read_lines "input" >>| parse in
  let lsum = fold ~init:0 ~f:(+) in
  let rec depth (T (_, _, xs)) = 1 + (xs >>| depth |> lsum) in
  let rec make node =
    let _, w, xs = find_exn ts ~f:(fun (x, _, _) -> String.(x = node)) in
    T (node, w, xs >>| make)
  in
  let T (root_name, _, _) as root = ts
    >>| (fun (x, _, _) -> make x)
    |> max_elt ~cmp:(fun x y -> depth x - depth y)
    |> Option.value_exn
  in
  printf "a) %s\n" root_name;
  let rec go (T (x, w, xs)) =
    let ys = xs >>| go in
    T (x, w + (ys >>| weight |> lsum), ys) in
  go root |> show |> print_endline
