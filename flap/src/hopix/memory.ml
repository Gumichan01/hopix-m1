

type address = HopixAST.label

type 'v t = (address * HopixAST.ty) list

let fresh () = [];;

let read _ _ _ = failwith "Students! This is your job!"
let read_block _ _ = failwith "Students! This is your job!"
let write _ _ _ _ = failwith "Students! This is your job!"

let allocate (memory : 'v t) (record : (HopixAST.label * HopixAST.ty) list) =
  let rec malloc m r =
    match r with
    | [] -> m
    | (l,v)::q -> malloc ((l,v)::m) q
  in
  malloc memory record
;;
  (* failwith "Students! This is your job!";; *)
