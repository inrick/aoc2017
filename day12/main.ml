open Base
open Stdio

let parse s =
  let open Caml.Scanf in
  let split s = List.(String.(split s ~on:',' >>| strip) >>| Int.of_string) in
  sscanf s "%d <-> %s@\n" (fun n xs -> n, split xs)

let () =
  let t = List.(In_channel.read_lines "input" >>| parse) in
  let n = List.length t in
  let g = Array.make_matrix ~dimx:n ~dimy:n false in
  List.iter t ~f:(fun (x, ys) -> List.iter ys ~f:(fun y ->
    g.(x).(y) <- true; g.(y).(x) <- true));
  let rec dfs visited = function
    | [] -> visited
    | x::xs when Set.mem visited x -> dfs visited xs
    | x::xs ->
      Array.foldi g.(x) ~init:xs ~f:(fun y to_visit edge ->
        if edge then y::to_visit else to_visit)
      |> dfs (Set.add visited x)
  in
  let empty = Set.empty (module Int) in
  let a = Set.length (dfs empty [0]) in
  printf "a) %d\n" a;
  let b =
    let rec go seen ngroups i =
      if i = n then ngroups
      else if Set.mem seen i then go seen ngroups (i+1)
      else go (dfs empty [i] |> Set.union seen) (ngroups+1) (i+1) in
    go empty 0 0 in
  printf "b) %d\n" b
