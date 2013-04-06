open Printf
open Math
open Scene

let leaf_max     = ref 4

(******************************************)
(* LOADING TRIANGLES                      *)
(******************************************)

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

let compute_order_x objects =
    let get_min_x (_,((min_x,_,_),(_,_,_))) = min_x
 in let order_x = Array.init (Array.length objects) (fun i->(get_min_x objects.(i),i))
 in let _ = Array.fast_sort (fun (av,_) (bv,_) -> compare av bv) order_x
 in Array.map (fun (_,i) -> i) order_x

let compute_order_y objects =
    let get_min_y (_,((_,min_y,_),(_,_,_))) = min_y
 in let order_y = Array.init (Array.length objects) (fun i->(get_min_y objects.(i),i))
 in let _ = Array.fast_sort (fun (av,_) (bv,_) -> compare av bv) order_y
 in Array.map (fun (_,i) -> i) order_y

let compute_order_z objects =
    let get_min_z (_,((_,_,min_z),(_,_,_))) = min_z
 in let order_z = Array.init (Array.length objects) (fun i->(get_min_z objects.(i),i))
 in let _ = Array.fast_sort (fun (av,_) (bv,_) -> compare av bv) order_z
 in Array.map (fun (_,i) -> i) order_z

let rec build_bb_tree_aux2 (bb_min,bb_max) obj_array order_x order_y order_z =
    if Array.length obj_array = 0
    then
	failwith "Semi-internal error: in build_bb_tree_aux(obj_array), obj_array has 0 size"
    else if Array.length obj_array = 1
    then
	let ((a,b,c,data),_) = obj_array.(0) in Triangle(a,b,c,data)
    else if Array.length obj_array <= (!leaf_max) (* default - 4 *)
    then    let objs = List.map (fun ((a,b,c,data),_) -> Triangle(a,b,c,data)) (Array.to_list obj_array)
	 in BBox(bb_null_stat(),bb_min,bb_max,objs)
    else    
    let get_bb (_,bb) = bb
 (* cost = surface area * number of objects inside
  * This cost function forces overbalancing
  * The actual cost is probably more like surface * log(number of objects inside)
  *)
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

let polygon_file = ref "bunny2.dat"
let output_file  = ref "scene.scm"

let arg_spec = [
    ("-o", Arg.String (fun arg -> output_file := arg), "output file");
    ("-s", Arg.String (fun arg -> polygon_file := arg), "polygon file");
    ("-k", Arg.Int    (fun arg -> leaf_max := arg), "max objects in leaf nodes")
]
let parse_rest str = failwith (sprintf "Parse error: argument %s not understood" str)

let usage_msg = "./build_kdtree.opt -s <source> -o <target>"
let _ = Arg.parse arg_spec parse_rest usage_msg

let green = (0.0, 1.0, 0.0)

let scene_file = open_in (!polygon_file)
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

let _ = save_scene (!output_file) scene
