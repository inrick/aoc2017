open Base
open Stdio

let next t =
  let open Array in
  let t = copy t in
  let n = length t in
  let m = reduce_exn t ~f:max in
  let j, _ = findi_exn t ~f:(fun _ x -> x = m) in
  t.(j) <- 0;
  for i = j+1 to j+m do
    t.(i%n) <- 1+t.(i%n)
  done;
  t

let () =
  let t =
    let open List in
    In_channel.read_lines "input"
    >>= String.split ~on:'\t'
    >>| Int.of_string
    |> to_array in
  let a, b =
    let open Hashtbl in
    let seen = Poly.create () in
    let rec go t step =
      match add seen ~key:t ~data:step with
      | `Duplicate -> step, step - find_exn seen t
      | `Ok -> go (next t) (step+1) in
    go t 0 in
  printf "a) %d\n" a;
  printf "b) %d\n" b
