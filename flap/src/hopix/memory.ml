

type address = int

type 'v t = (address * (HopixAST.label * HopixAST.ty) list) list

let x = ref 1;;

let fresh () = [];;

let read _ _ _ = failwith "Students! This is your job!"
let read_block (memory: 'v t) (adr : address) =
  match memory with
  | [] -> []
  | _ -> List.assoc adr memory

(*failwith "Students! This is your job!"*)


let write _ _ _ _ = failwith "Students! This is your job!"

let allocate (memory : 'v t) (record : (HopixAST.label * HopixAST.ty) list) =
  let y = !x in x := !x + 1; (y,record)::memory
;;
