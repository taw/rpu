open Printf
open Scene

(******************************************)
(* AUX FUNCTIONS                          *)
(******************************************)

let max3 a b c = max (max a b) c
let min3 a b c = min (min a b) c

(*
 * The difference between list_eager_for_any and List.exists
 * is that list_eager_for_any evaluates f x for all x on the list,
 * even if the result is known.
 * It's important for mixed imperative/functional programming
 *)
let rec list_eager_for_any f = function
  | (hd::tl)-> if f hd
	       then (let _ = List.iter (fun x -> ignore (f x)) tl in true)
	       else list_eager_for_any f tl
  | []      -> false

(******************************************)
(* 3-VECTOR OPERATIONS                    *)
(******************************************)
let (+&)    (ax,ay,az) (bx,by,bz) = (ax+.bx, ay+.by, az+.bz)
let (-&)    (ax,ay,az) (bx,by,bz) = (ax-.bx, ay-.by, az-.bz)
let ( *& )  (ax,ay,az) (bx,by,bz) = (ax*.bx +. ay*.by +. az*.bz)
let ( *&& ) (ax,ay,az) (bx,by,bz) = ((ay*.bz)-.(az*.by), (az*.bx)-.(ax*.bz), (ax*.by)-.(ay*.bx))
let scnorm a = sqrt (a *& a)
let ( *&* ) (ax,ay,az) b = (ax*.b, ay*.b, az*.b)
let ( /& )  (ax,ay,az) b = (ax/.b, ay/.b, az/.b)
(* norm(0) = 0 ? NaN ? undefined ? *)
let v3norm a = a /& (scnorm a)

(******************************************)
(* SCENE LOADING                          *)
(******************************************)

let red   = (1.0, 0.0, 0.0)
let green = (0.0, 1.0, 0.0)
let blue  = (1.0, 0.0, 1.0)
let lime  = (0.0, 1.0, 1.0)
let black = (0.0, 0.0, 0.0)

(* Simple test if it's worth checking
 * If check     ^ hit               -> set to true
 * If not check ^ all children miss -> set to false
 *)
let bb_null_stat () = ref true

let bb1d a b c =
    if a < b
    then ((min a c), (max b c))
    else ((min b c), (max a c))
let triangle_bounding_box (ax,ay,az) (bx,by,bz) (cx,cy,cz) =
    let (x0,x1) = bb1d ax bx cx
 in let (y0,y1) = bb1d ay by cy
 in let (z0,z1) = bb1d az bz cz
 in ((x0,y0,z0),(x1,y1,z1))

(*
 * The idea is that:
 * bb_union null_bb x = x
 *)
let bb_union ((a_min_x,a_min_y,a_min_z),(a_max_x,a_max_y,a_max_z))
	     ((b_min_x,b_min_y,b_min_z),(b_max_x,b_max_y,b_max_z)) =
	     (
	        ((min a_min_x b_min_x), (min a_min_y b_min_y), (min a_min_z b_min_z)),
		((max a_max_x b_max_x), (max a_max_y b_max_y), (max a_max_z b_max_z))
	     )
let null_bb = ((infinity, infinity, infinity), (neg_infinity, neg_infinity, neg_infinity))

let cmp a b = if a > b then 1 else if a = b then 0 else -1

let compute_order_x objects =
    let get_min_x (_,((min_x,_,_),(_,_,_))) = min_x
 in let order_x = Array.init (Array.length objects) (fun i->(get_min_x objects.(i),i))
 in let _ = Array.fast_sort (fun (av,ai) (bv,bi) -> cmp av bv) order_x
 in Array.map (fun (_,i) -> i) order_x

let compute_order_y objects =
    let get_min_y (_,((_,min_y,_),(_,_,_))) = min_y
 in let order_y = Array.init (Array.length objects) (fun i->(get_min_y objects.(i),i))
 in let _ = Array.fast_sort (fun (av,ai) (bv,bi) -> cmp av bv) order_y
 in Array.map (fun (_,i) -> i) order_y

let compute_order_z objects =
    let get_min_z (_,((_,_,min_z),(_,_,_))) = min_z
 in let order_z = Array.init (Array.length objects) (fun i->(get_min_z objects.(i),i))
 in let _ = Array.fast_sort (fun (av,ai) (bv,bi) -> cmp av bv) order_z
 in Array.map (fun (_,i) -> i) order_z

let rec build_bb_tree_aux2 (bb_min,bb_max) obj_array order_x order_y order_z =
    if Array.length obj_array = 0
    then
	failwith "Semi-internal error: in build_bb_tree_aux(obj_array), obj_array has 0 size"
    else if Array.length obj_array = 1
    then
	let ((a,b,c,data),_) = obj_array.(0) in Triangle(a,b,c,data)
    else if Array.length obj_array <= 4
    then    let objs = List.map (fun ((a,b,c,data),_) -> Triangle(a,b,c,data)) (Array.to_list obj_array)
	 in BBox(bb_null_stat(),bb_min,bb_max,objs)
    else    
    let get_bb (_,bb) = bb
 (* cost = surface area *)
 in let bb_cost ((a0,b0,c0),(a1,b1,c1)) obj_cnt =
       let (asz,bsz,csz) = (a1-.a0),(b1-.b0),(c1-.c0)
    in ((asz*.bsz) +. (asz*.csz) +. (bsz*.csz)) *. (float_of_int obj_cnt)
 (* There are only N-1 possible split points, not N *)
 in let best_split = ref None
 in let record_new_split cost split_data = match !best_split with
    | None -> best_split := Some(cost,split_data)
    | Some (best_cost,_) -> if cost < best_cost
                            then best_split := Some (cost,split_data)
 in let try_scores index_array =
	   let last_bb  = ref null_bb
	in let bb_left  = Array.make (Array.length obj_array-1) null_bb
	in let bb_right = Array.make (Array.length obj_array-1) null_bb
        in let cost = Array.init (Array.length obj_array-1)
	    (fun i->
		let cur_bb = get_bb obj_array.(index_array.(i))
	     in let new_bb = bb_union (!last_bb) cur_bb
	     in let _ = last_bb := new_bb
	     in let _ = bb_left.(i) <- new_bb
	     in bb_cost new_bb (i+1)
	    )
	in let last_bb = ref null_bb
	in let _ =
	   for i = Array.length obj_array-1 downto 1 do
		let cur_bb = get_bb obj_array.(index_array.(i))
	     in let new_bb = bb_union (!last_bb) cur_bb
	     in let _ = last_bb := new_bb
	     in let _ = bb_right.(i-1) <- new_bb
	     in cost.(i-1) <- cost.(i-1) +. bb_cost new_bb (Array.length obj_array - i)
	   done
	in for i = 0 to Array.length obj_array-2 do
	     record_new_split cost.(i) (index_array, i+1, bb_left.(i), bb_right.(i))
	   done
 in let _ = try_scores order_x
 in let _ = try_scores order_y
 in let _ = try_scores order_z
 in let split = match !best_split with
	      | None -> failwith "Internal error: at least one split should be possible"
	      | Some (_,split_data) -> split_data
 in let n = Array.length obj_array
 in let (order,left_size,bb_left,bb_right) = split
 
 in let split_order_rev = Array.make n (-1)
 in let _ = for i = 0 to n-1 do split_order_rev.(order.(i)) <- i  done
 in let right_size = n - left_size

 in let left_order_x     = Array.make left_size (-1)
 in let left_order_x_sz  = ref 0
 in let right_order_x    = Array.make right_size (-1)
 in let right_order_x_sz = ref 0
 in let _ =
    for i = 0 to n-1 do
	   let x_a = split_order_rev.(order_x.(i))
	in if x_a < left_size
	   then (left_order_x.(!left_order_x_sz) <- x_a; left_order_x_sz := !left_order_x_sz + 1)
	   else (right_order_x.(!right_order_x_sz) <- x_a-left_size; right_order_x_sz := !right_order_x_sz + 1)
    done

 in let left_order_y     = Array.make left_size (-1)
 in let left_order_y_sz  = ref 0
 in let right_order_y    = Array.make right_size (-1)
 in let right_order_y_sz = ref 0
 in let _ =
    for i = 0 to n-1 do
	   let y_a = split_order_rev.(order_y.(i))
	in if y_a < left_size
	   then (left_order_y.(!left_order_y_sz) <- y_a; left_order_y_sz := !left_order_y_sz + 1)
	   else (right_order_y.(!right_order_y_sz) <- y_a-left_size; right_order_y_sz := !right_order_y_sz + 1)
    done

 in let left_order_z     = Array.make left_size (-1)
 in let left_order_z_sz  = ref 0
 in let right_order_z    = Array.make right_size (-1)
 in let right_order_z_sz = ref 0
 in let _ =
    for i = 0 to n-1 do
	   let z_a = split_order_rev.(order_z.(i))
	in if z_a < left_size
	   then (left_order_z.(!left_order_z_sz) <- z_a; left_order_z_sz := !left_order_z_sz + 1)
	   else (right_order_z.(!right_order_z_sz) <- z_a-left_size; right_order_z_sz := !right_order_z_sz + 1)
    done

 in let left_objects  = Array.init left_size (fun i -> obj_array.(order.(i)))
 in let right_objects = Array.init right_size (fun i -> obj_array.(order.(left_size+i)))

 in let left_obj  = build_bb_tree_aux2 bb_left left_objects left_order_x left_order_y left_order_z
 in let right_obj = build_bb_tree_aux2 bb_right right_objects right_order_x right_order_y right_order_z

 (* [right_obj; left_obj] results in about 2% BB/ray intersection tests for the bunny scene
  * than [left_obj; right_obj].
  *)
 in BBox(bb_null_stat(),bb_min,bb_max,[right_obj; left_obj])

let build_bb_tree_aux obj_array =
    let global_bb = List.fold_left (fun bb0 (_,bb1) -> bb_union bb0 bb1) null_bb obj_array
 in let obj_array = Array.of_list obj_array
 in let order_x = compute_order_x obj_array
 in let order_y = compute_order_y obj_array
 in let order_z = compute_order_z obj_array
 in build_bb_tree_aux2 global_bb obj_array order_x order_y order_z

let bb_triangle (((ax,ay,az),(bx,by,bz),(cx,cy,cz),data) as obj) =
    (obj,(
	((min3 ax bx cx),(min3 ay by cy),(min3 az bz cz)),
	((max3 ax bx cx),(max3 ay by cy),(max3 az bz cz))
    ))
let build_bb_tree triangles = build_bb_tree_aux (List.map bb_triangle triangles)

(******************************************)
(* Scene loading                          *)
(******************************************)

(* LOCAL *)
let scene_file = open_in "bunny2.dat";;
(* LOCAL *)
let triangles =
    let scene_ref = ref []
 in let scan_line () =
	try
	    Scanf.fscanf scene_file "%f %f %f %f %f %f %f %f %f" (
		fun ax ay az bx by bz cx cy cz ->
	            Some ((ax,ay,az),(bx,by,bz),(cx,cy,cz),green)
	    )
	with End_of_file -> None
 in let rec scan_file () =
        match scan_line() with
        | None -> ()
	| Some obj -> (scene_ref := obj::!scene_ref; scan_file())
 in let _ = scan_file ()
 (* Order doesn't matter, but it simplifies debugging *)
 in List.rev !scene_ref
let scene = build_bb_tree triangles

let save_scene scene = output_value stdout scene

let _ = save_scene scene
