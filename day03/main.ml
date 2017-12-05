open Base
open Stdio

let (+|) (a,b) (c,d) = a+c, b+d

let walk () =
  let dirs = [|1,0; 0,1; -1,0; 0,-1|] in
  (* position, repeat, step, until, direction *)
  Sequence.unfold ~init:((0,0), 0, 0, 1, 0) ~f:(fun (pos, r, s, n, d) ->
    let pos' = pos +| dirs.(d) in
    let r' = r+1 in
    let s' = if r' = n then s+1 else s in
    let n' = if s' = 2 then n+1 else n in
    let d' = if s <> s' then (d+1)%4 else d in
    Some (pos', (pos', r'%n, s'%2, n', d')))

let sum_of_neighbors () =
  let neighbors pos =
    List.([1,0; 1,1; 0,1; -1,1; -1,0; -1,-1; 0,-1; 1,-1] >>| ((+|) pos))
  in
  let h = Hashtbl.Poly.create () in
  let add pos v = Hashtbl.add_exn h ~key:pos ~data:v in
  let value pos = match Hashtbl.find h pos with Some v -> v | None -> 0 in
  add (0,0) 1;
  fun pos ->
    let v = List.(neighbors pos >>| value |> fold ~init:0 ~f:(+)) in
    add pos v;
    v

let () =
  let open Sequence in
  let input = 289326 in
  let dist0 (a,b) = abs a + abs b in
  walk ()
    >>| dist0
    |> Fn.flip nth_exn (input-2) (* origin excluded + zero indexed *)
    |> printf "a) %d\n";
  walk ()
    >>| sum_of_neighbors ()
    |> drop_while ~f:(fun x -> x < input)
    |> hd_exn
    |> printf "b) %d\n";
