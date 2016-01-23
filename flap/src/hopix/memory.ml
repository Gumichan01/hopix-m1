

type address = int

type 'v t = (address * (HopixAST.label * 'v) list) list

let x = ref 1;;

let fresh () = [];;

let allocate (memory : 'v t) (record : (HopixAST.label * 'v) list) =
  let y = !x in x := !x + 1; (y,record)::memory
;;


let read_block (memory: 'v t) (addr : address) =
  try
    List.assoc addr memory
  with Not_found -> []
;;


let read (memory : 'v t) (addr : address) (lab : HopixAST.label) =
  let ty = List.assoc lab (read_block memory addr) in ty
;;

let write mem addr lab v =
  let rec_list = read_block mem addr in
  let rec write_aux lb = function
    | [] -> []
    | (k,c)::q -> if (lb = k) then (k,v)::q else (k,c)::(write_aux lb q)
  in let y = !x in x := !x + 1; (y,(write_aux lab rec_list))::mem
;;

