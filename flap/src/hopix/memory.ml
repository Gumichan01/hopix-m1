

type address = int

type 'v t = (address * (HopixAST.label * 'v) list) list

let x = ref 1;;

let fresh () = [];;

let read_block (memory: 'v t) (adr : address) =
  try
    List.assoc adr memory
  with Not_found -> []
;;


let read (memory : 'v t) (adr : address) (lab : HopixAST.label) =
  let ty = List.assoc lab (read_block memory adr) in ty
;;

let write _ _ _ _ = failwith "Students! This is your job!"


let allocate (memory : 'v t) (record : (HopixAST.label * 'v) list) =
  let y = !x in x := !x + 1; (y,record)::memory
;;
