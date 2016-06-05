

type address = int

type 'v t = (address * (HopixAST.label * 'v) list) list

let x = ref 1;;

let fresh () = [];;

let allocate (memory : 'v t) (record : (HopixAST.label * 'v) list) =
  let y = !x in x := !x + 1; (y,(y,record)::memory)
;;


let read_block (memory: 'v t) (addr : address) =
  try
    List.assoc addr memory
  with Not_found -> []
;;


let read (memory : 'v t) (addr : address) (lab : HopixAST.label) =
  let ty = List.assoc lab (read_block memory addr) in ty
;;

let write (mem : 'v t) addr lab v =
  (* Go into a block *)
  let rec write_ lb v (bl : (HopixAST.label * 'v) list) =
    match bl with
    | [] -> []
    | (k,c)::q ->
      begin
        if (lb = k)
        then (k,v)::q
        else (k,c)::(write_ lb v q)
      end
  (* Go into the memory *)
  and write_aux (m : 'v t) addr lab v : 'v t =
  match m with
  | [] -> []
  | (a,block)::memq ->
    if addr = a
    then (a,(write_ lab v block))::memq
    else (a,block)::(write_aux memq addr lab v)
  in write_aux mem addr lab v
;;
