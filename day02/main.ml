open Base
open Stdio

let () =
  let open List in
  let t =
    In_channel.read_lines "input"
    >>| String.split ~on:'\t'
    >>| map ~f:Int.of_string
  in
  let f1 xs = reduce_exn ~f:max xs - reduce_exn ~f:min xs in
  let f2 xs =
    cartesian_product xs xs
    |> filter_map ~f:(fun (x, y) ->
      if x <> y && x % y = 0 then Some (x/y) else None)
    |> hd_exn
  in
  let sum = fold ~init:0 ~f:(+) in
  printf "a) %d\n" (t >>| f1 |> sum);
  printf "b) %d\n" (t >>| f2 |> sum)
