open Base
open Stdio

let strength (x,y) = x+y

let max_strength (m1,_) (m2,_) = max m1 m2, 0 (* don't care about length *)

let max_length (m1,l1) (m2,l2) =
  if l2 > l1 then m2, l2
  else if l2 = l1 then max m1 m2, l2
  else m1, l1

let () =
  let parse s = Caml.Scanf.sscanf s "%d/%d" (fun x y -> x, y) in
  let t = List.(In_channel.read_lines "input" >>| parse |> to_array) in
  let len = Array.length t in
  let marked = Array.create ~len false in
  let rec go max_by ~mark:i ~unused =
    marked.(i) <- true;
    let m, l = Array.foldi t ~init:(0,0) ~f:(fun i acc (x, y) ->
      if (unused = x || unused = y) && not marked.(i) then
        max_by acc (go max_by i (if unused = x then y else x))
      else acc) in
    marked.(i) <- false;
    m + strength t.(i), l+1 in
  let solve max_by =
    Array.filter_mapi t ~f:(fun i -> function
      | 0, x | x, 0 -> Some (go max_by ~mark:i ~unused:x)
      | _ -> None)
    |> Array.fold ~init:(0,0) ~f:max_by
    |> fst in
  let a = solve max_strength in
  printf "a) %d\n" a;
  let b = solve max_length in
  printf "b) %d\n" b
