(*
* Does not work - NaNs are normalized
let int32_to_float : int32 -> float = Int32.float_of_bits
let float_to_int32 : float -> int32 = Int32.bits_of_float
*)
let int32_to_float : int32 -> float = Int32.to_float
let float_to_int32 : float -> int32 = Int32.of_float

let int32_embed_cycle : int32 -> int32 = fun x -> float_to_int32 (int32_to_float x)
let int32_embed_ok : int32 -> bool = fun x -> (int32_embed_cycle x) = x
;;
let rec check_all x =
    if int32_embed_ok x
    then
    (
	if Int32.logand x 0xFFFFl = 0l
	then Printf.printf "OK %lx -> %lx\n" x (int32_embed_cycle x);
	if x <> 0l
	then check_all (Int32.pred x);
    )
    else
	Printf.printf "Died on %lx -> %lx\n" x (int32_embed_cycle x)
;;
check_all 0xFFFFFFFFl;;
