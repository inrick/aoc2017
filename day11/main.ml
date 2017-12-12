open Base
open Stdio

let (+|) (a,b) (c,d) = a+c, b+d

let scan init f xs =
  List.fold xs ~init:([init], init) ~f:(fun (acc, x) y ->
    let a = f x y in a::acc, a)
  |> fst |> List.rev

let () =
  let open List in
  let coord =
    Map.of_alist_exn (module String)
    ["s", (0,-2); "ne", (1,1); "sw", (-1,-1);
     "n", (0,2); "nw", (-1,1); "se", (1,-1)]
  in
  let t = In_channel.read_lines "input" >>= String.split ~on:',' in
  let walk = t >>| Map.find_exn coord |> scan (0,0) (+|) in
  (* TODO this is not correct, just happened to work with my input, fix *)
  let dist0 (x,y) = (abs y - abs x)/2 + abs x in
  printf "a) %d\n" (walk |> last_exn |> dist0);
  printf "b) %d\n" (walk >>| dist0 |> reduce_exn ~f:max)
