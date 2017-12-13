open Base
open Stdio

type state = { pos : int; range : int; f : int -> int }

let () =
  let t =
    let open List in
    In_channel.read_lines "input"
    >>| String.split ~on:':'
    >>| List.map ~f:(Fn.compose Int.of_string String.strip)
    >>| function [x;y] -> x, y | _ -> assert false in
  let n = 1 + fst (List.last_exn t) in
  let update { pos; range; f } =
    let f =
      if f pos = range then fun x -> x - 1
      else if f pos = -1 then fun x -> x + 1
      else f in
    let pos = f pos in
    { pos; range; f } in
  let update = Option.map ~f:update in
  let shifted =
    let init = Array.create ~len:n None in
    List.iter t ~f:(fun (x,y) -> init.(x) <- Some { pos=0; range=y; f=(+) 1 });
    Array.mapi init ~f:(fun i -> Fn.apply_n_times ~n:i update)
  in
  let a = Array.foldi shifted ~init:0 ~f:(fun i sev -> function
    | Some { pos; range; _ } when pos = 0 -> sev+i*range
    | None | Some _ -> sev) in
  printf "a) %d\n" a;
  let rec find_delay state i =
    if Array.for_all state ~f:(function None -> true | Some x -> x.pos <> 0)
    then i
    else find_delay (Array.map state ~f:update) (i+1)
  in
  printf "b) %d\n" (find_delay shifted 0)
