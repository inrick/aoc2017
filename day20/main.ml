open Base
open Stdio

type vec = {x : int; y : int; z : int}
type particle = {p : vec; v : vec; a : vec}

let parse s =
  let open Caml.Scanf in
  sscanf s "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>"
    (fun px py pz vx vy vz ax ay az ->
      {p={x=px;y=py;z=pz}; v={x=vx;y=vy;z=vz}; a={x=ax;y=ay;z=az}})

let dist0 {p={x;y;z}; _} = abs x + abs y + abs z

let tick {p; v; a} =
  let vx = v.x + a.x and vy = v.y + a.y and vz = v.z + a.z in
  let px = p.x + vx  and py = p.y + vy  and pz = p.z + vz in
  {p={x=px;y=py;z=pz}; v={x=vx;y=vy;z=vz}; a}

let collide {p=p0; _} {p=p1; _} =
  p0.x = p1.x && p0.y = p1.y && p0.z = p1.z

let () =
  let t = List.(In_channel.read_lines "input" >>| parse |> to_array) in
  (* TODO figure out answer instead of iterating a fixed number of times *)
  let a =
    let rec go t n =
      let open Array in
      if n < 1000 then go (map t ~f:tick) (n+1) else
        let distances = map t ~f:dist0 in
        let m = min_elt distances ~cmp:Int.compare |> Option.value_exn in
        findi_exn distances ~f:(fun _ -> (=) m) |> fst in
    go t 0 in
  printf "a) %d\n" a;
  let b =
    let tick = Option.map ~f:tick in
    let rec go t n =
      let open Array in
      if n = 100 then count t ~f:Option.is_some else begin
        let t = map t ~f:tick in
        let len = length t in
        for i = 0 to len-1 do
          let p0 = t.(i) in
          for j = 0 to len-1 do
            let collision = Option.map2 p0 t.(j) ~f:collide
              |> Option.value ~default:false in
            if i <> j && collision then begin
              t.(i) <- None; t.(j) <- None;
            end
          done
        done;
        go t (n+1)
      end in
    go (Array.map t ~f:Option.some) 0 in
  printf "b) %d\n" b
