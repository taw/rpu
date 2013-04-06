(* 
 * Because of the readability issues the code has a blanket ban on\
 * using Hashtbl.* DynArray.* etc. functions directly.
 * Everything should have a standarized functions here
 *
 * There's also a blanket ban on using folds
 *
 * It makes the code look just a bit more readable
 *
 * This file also includes small utility functions used throughout the code
 *
 * The names are much more consistent and match Ruby nomenclature,
 * used also in other languages like Python/Java/etc.
 *
 * Also:
 * * Ocaml ('a,'b) Hashtbl.t is used in 3 different functions
 * * as a 'a set
 * * as 'a->'b mapping
 * * as 'a->('b list) mapping
 * so they are split to 'a set, 'a ht, 'a mht
 *
 * Oh, and tuples with >3 elements are STRONGLY discouraged
 *)

exception Not_unique

(* Wrap them, so they present a nice encapsulated interface
 * to the outside world *)
type ('a,'b) ht  = HT of ('a,'b) Hashtbl.t
type ('a,'b) mht = MHT of ('a,'b) Hashtbl.t
type 'a set = SET of ('a,unit) Hashtbl.t

type 'a da = 'a DynArray.t 

let ht_new  ()  = HT(Hashtbl.create 0)
let mht_new () = MHT(Hashtbl.create 0)
let set_new () = SET(Hashtbl.create 0)
let da_new  ()  = DynArray.create ()

let ht_get (HT ht) k = Hashtbl.find ht k
let da_get da k = DynArray.get da k

let ht_get_orelse ht k f =
	try ht_get ht k
	with Not_found -> f ()

let ht_get_with_default ht k default_value =
	try ht_get ht k
	with Not_found -> default_value

let da_size da = DynArray.length da

let da_to_list da = DynArray.to_list da

let da_of_list lst = DynArray.of_list lst

let ht_clear (HT ht)   = Hashtbl.clear ht
let mht_clear (MHT ht) = Hashtbl.clear ht
let set_clear (SET st) = Hashtbl.clear st
let da_clear da = DynArray.clear da

let ht_del (HT ht) k = Hashtbl.remove ht k

(* FIXME: replace with mht *)
let mht_get_all (MHT ht) k = Hashtbl.find_all ht k

let ht_set (HT ht) k v = Hashtbl.replace ht k v
let da_set da k v = DynArray.set da k v

(*
 * NOTICE:
 * Use a "normal" format: method object args
 * instead of a reversed one, like in DynArray !
 *)
let da_append_da a b = DynArray.append b a

let set_set (SET st) k = Hashtbl.replace st k ()

let set_del (SET st) k = Hashtbl.remove st k

let ht_mem (HT ht) k = Hashtbl.mem ht k
let set_mem (SET st) k = Hashtbl.mem st k

let da_iter f da = DynArray.iter f da
let ht_iter f (HT ht) = Hashtbl.iter f ht
let set_iter f (SET ht) = Hashtbl.iter (fun k _ -> f k) ht


let ht_dup (HT ht) = HT(Hashtbl.copy ht)

let da_push da e = DynArray.add da e

let mht_add (MHT ht) k v = Hashtbl.add ht k v

let ht_contents  : ('a,'b) Hashtbl.t -> ('a*'b) list
= fun ht -> Hashtbl.fold (fun key valu contents -> (key,valu)::contents) ht []

let ht_keys : ('a,'b) ht -> 'a list
= fun (HT ht) -> Hashtbl.fold (fun key _    contents -> key::contents) ht []

let ht_values : ('a,'b) ht -> 'b list
= fun (HT ht) -> Hashtbl.fold (fun _   valu contents -> valu::contents) ht []

let ht_set_unique (HT ht) k v =
    try (
		Hashtbl.find ht k;
		raise Not_unique
    )
    with Not_found ->
		Hashtbl.replace ht k v
let ht_set_unique_or ht k v f =
    try (
		ht_set_unique ht k v
    )
    with Not_unique ->
    	f()

let mht_iter_all f (MHT ht) =
    let seen = set_new () in
    Hashtbl.iter (fun k _ ->
	if set_mem seen k
	then ()
	else
	(
	    set_set seen k;
	    f k (Hashtbl.find_all ht k)
	)
    ) ht

(* For better debugability, call all (k,v0) (k,v1) ... (k,vn) together *)
let mht_iter1 f mht =
	mht_iter_all (fun k vs ->
		List.iter (fun v -> f k v) vs
	) mht

let mht_get_unique mht k =
	match mht_get_all mht k with
	| []  -> raise Not_found
	| [v] -> v
	| _   -> raise Not_unique

let ht_collect : ('a -> 'b -> 'c option) -> ('a,'b) ht -> 'c list
= fun f (HT ht) ->
    Hashtbl.fold (fun k v r ->
	match f k v with
	| None   -> r
	| Some x -> x::r
    ) ht []
(*
 * (da_imprerative_map f) da does the same thing
 * as (List.map f da), but works in-place on 'a da
 *)
let da_imperative_map : ('a->'a) -> ('a DynArray.t) -> unit
= fun f da ->
    for i = 0 to (da_size da - 1) do
		let new_value = f (da_get da i) in
		da_set da i new_value
    done

(*
 * (da_imprerative_filter f da) does the same thing
 * as (List.filter f da), but works in-place on 'a da
 *)
let da_imperative_filter : ('a->bool) -> ('a DynArray.t) -> unit
= fun f da ->
    let lst = da_to_list da in
    let clst = List.filter f lst in
    da_clear da;
    List.iter (fun x -> DynArray.add da x) clst

(*
 * This is a common "folding" pattern:
 * list_map_with_state [a0..an] s0 = ([b0..bn], s(n+1))
 * where (bi, s(i+1)) = f (ai, si)
 *)
let list_map_with_state : ('a*'c -> 'b*'c) -> 'a list -> 'c -> ('b list * 'c)
= fun f lst state0 ->
    let rec aux t state = function
    | hd::tl -> let (hdm,state) = f(hd,state) in aux (hdm::t) state tl
    | []     -> (List.rev t, state)
    in aux [] state0 lst

let rec list_join sep = function
| []    -> ""
| a::[] -> a
| a::tl -> a^sep^(list_join sep tl)

let list_iteri f lst = Array.iteri f (Array.of_list lst)

let list_union : 'a list list -> 'a list
= fun lsts ->
    let ht = ht_new () in
    List.iter (fun lst ->
	List.iter (fun el ->
	    ht_set ht el ()
	) lst
    ) lsts;
    ht_keys ht

let ht_sorted_iter : ('a->'b->unit) -> ('a,'b) ht -> unit
= fun f ht ->
    let keys = ht_keys ht
 in let keys = List.rev (List.sort compare keys)
 in List.iter (fun k ->
	f k (ht_get ht k)
 ) keys
