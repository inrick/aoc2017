open Base
open Stdio

let (+|) (a,b) (c,d) = a+c, b+d

let walk () =
  let open Sequence in
  let dirs = [|1,0; 0,1; -1,0; 0,-1|] in
  let dir i = dirs.(i%4) in
  let scan init f =
    unfold_with ~init ~f:(fun x y -> let z = f x y in Step.Yield (z, z))
  in
  let where = concat_mapi ~f:(fun i x -> take (repeat i) x) in
  repeat 2 |> where >>| (+) 1 |> where >>| dir |> scan (0,0) (+|)

let sum_of_neighbors () =
  let neighbors pos =
    List.([1,0; 1,1; 0,1; -1,1; -1,0; -1,-1; 0,-1; 1,-1] >>| (+|) pos)
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
