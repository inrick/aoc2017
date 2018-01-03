open Base
open Stdio

let rot mat =
  let len = Array.length mat in
  Array.init len ~f:(fun i -> String.init len ~f:(fun j -> mat.(j).[len-1-i]))

let flipx mat =
  let len = Array.length mat in
  Array.init len ~f:(fun i -> mat.(len-1-i))

let flipy mat = Array.map mat ~f:String.rev

let rots mat = List.(range 0 4 >>| fun n -> Fn.apply_n_times ~n rot mat)

let moves mat = List.bind ~f:rots [mat; flipx mat; flipy mat]

let print_mat mat = Array.iter mat ~f:print_endline

let msplit mat =
  let open Array in
  let len = length mat in
  let m = if len % 2 = 0 then 2 else 3 in (* mxm squares *)
  let n = len / m in
  init n ~f:(fun i ->
    init n ~f:(fun j ->
      init m ~f:(fun k ->
        String.slice mat.(k+m*i) (m*j) (m*j+m))))

let mjoin mats =
  let open Array in
  let m = length mats.(0).(0) in
  let n = length mats in
  init (n*m) ~f:(fun i ->
    let d, r = i/m, i%m in
    map mats.(d) ~f:(fun x -> x.(r)) |> to_list |> String.concat)

let count_pixels mat =
  Array.map mat ~f:(String.count ~f:(Char.(=) '#'))
  |> Array.fold ~init:0 ~f:(+)

let () =
  let open List in
  let t = In_channel.read_lines "input"
    >>| String.split ~on:' '
    >>| to_array
    >>| (fun x -> x.(0), x.(2))
    |> Map.of_alist_exn (module String) in
  let init = [|".#.";"..#";"###"|] in
  let of_mat mat = Array.to_list mat |> String.concat ~sep:"/" in
  let to_mat str = String.split ~on:'/' str |> Array.of_list in
  let enhance mat =
    moves mat >>| of_mat >>| Map.find t >>= Option.to_list |> hd_exn |> to_mat
  in
  let next mat = msplit mat |> Array.map ~f:(Array.map ~f:enhance) |> mjoin in
  let mat = Fn.apply_n_times ~n:5 next init in
  (*print_mat mat;*)
  printf "a) %d\n" (count_pixels mat);
  let mat2 = Fn.apply_n_times ~n:18 next init in
  printf "b) %d\n" (count_pixels mat2)
