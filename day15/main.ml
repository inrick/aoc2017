open Base
open Stdio

let () =
  let open Sequence in
  let step fact x = (x * fact) % 2_147_483_647 (* 2^31 - 1 *) in
  let eq_low16 (x,y) = x land 0xffff = y land 0xffff in
  let unfold_simple init f =
    unfold ~init ~f:(fun x -> let y = f x in Some (y,y)) in
  (* explicit loop is significantly faster but this looks quite nice *)
  let seq_a = unfold_simple 277 (step 16807) in
  let seq_b = unfold_simple 349 (step 48271) in
  let a = zip seq_a seq_b |> Fn.flip take 40_000_000 |> count ~f:eq_low16 in
  printf "a) %d\n" a;
  let seq_a2 = filter seq_a ~f:(fun x -> x % 4 = 0) in
  let seq_b2 = filter seq_b ~f:(fun x -> x % 8 = 0) in
  let b = zip seq_a2 seq_b2 |> Fn.flip take 5_000_000 |> count ~f:eq_low16 in
  printf "b) %d\n" b
