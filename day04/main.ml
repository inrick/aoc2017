open Base
open Stdio

let () =
  let open List in
  let t = In_channel.read_lines "input" >>| String.split ~on:' ' in
  let sort_string s =
    String.to_list s |> sort ~cmp:Char.compare |> String.of_char_list
  in
  printf "a) %d\n" (t >>| contains_dup |> count ~f:not);
  printf "b) %d\n" (t >>| map ~f:sort_string >>| contains_dup |> count ~f:not)
