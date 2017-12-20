open Base
open Stdio

type dir = U | D | L | R

let move (r,c) = function
  | U -> r-1, c | L -> r, c-1
  | D -> r+1, c | R -> r, c+1

let () =
  let field = List.(In_channel.read_lines "input" >>| String.to_array)
    |> Array.of_list in
  let c0 = Array.findi_exn field.(0) (fun _ -> Char.(=) '|') |> fst in
  let get (r,c) = try field.(r).(c) with _ -> ' ' in
  let next dir pos acc =
    match get pos with
    | ' ' -> None (* this is the end *)
    | '-' | '|' -> Some (dir, acc)
    | 'A'..'Z' as x -> Some (dir, x::acc)
    | '+' ->
      let dir =
        begin match dir with
        | U -> [U;L;R] | L -> [U;D;L]
        | D -> [D;L;R] | R -> [U;D;R]
        end
        |> List.filter ~f:(fun x -> Char.(get (move pos x) <> ' '))
        |> List.hd_exn in
      Some (dir, acc)
    | _ -> assert false in
  let rec go dir pos acc n =
    match next dir pos acc with
    | Some (dir, acc) -> go dir (move pos dir) acc (n+1)
    | None -> String.of_char_list (List.rev acc), n in
  let s, n = go D (0,c0) [] 0 in
  printf "a) %s\n" s;
  printf "b) %d\n" n
