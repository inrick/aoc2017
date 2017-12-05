open Base
open Stdio

let solve t f =
  let t = Array.of_list t in
  let n = Array.length t in
  let rec go i s =
    if i < 0 || n <= i then s
    else begin
      let j = t.(i) in
      t.(i) <- f j;
      go (i+j) (s+1)
    end in
  go 0 0

let () =
  let t = In_channel.read_lines "input" |> List.map ~f:Int.of_string in
  printf "a) %d\n" (solve t ((+) 1));
  printf "b) %d\n" (solve t (fun x -> if x<3 then x+1 else x-1))
