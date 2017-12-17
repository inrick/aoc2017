open Base
open Stdio

let insert xs x i =
  let open Array in
  let n = length xs in
  let i = i+1 in
  let ys = create ~len:(n+1) x in
  blit ~src:xs ~src_pos:0 ~dst:ys ~dst_pos:0 ~len:i;
  blit ~src:xs ~src_pos:i ~dst:ys ~dst_pos:(i+1) ~len:(n-i);
  ys

let () =
  let step = 382 in
  let a =
    let rec go n pos xs =
      if n = 2018 then xs
      else
        let pos = (1+step+pos) % n in
        go (n+1) pos (insert xs n pos) in
    let xs = go 1 0 [|0|] in
    let i, _ = Array.findi_exn xs ~f:(fun _ -> (=) 2017) in
    xs.((i+1)%2018) in
  printf "a) %d\n" a;
  let b =
    let rec go after0 n pos =
      if n = 50_000_000 then after0
      else
        let pos = (1+step+pos) % n in
        let after0 = if pos = 0 then n else after0 in
        go after0 (n+1) pos in
    go 0 1 0 in
  printf "b) %d\n" b
