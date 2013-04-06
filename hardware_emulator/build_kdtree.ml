open Printf
open Math3

(* Triangle or BBox index *)
type sceneobj = T of int | B of int
type ('a,'b) scene =
    {
	vertices  : (v3 * 'a) DynArray.t;
	tridata   : 'b DynArray.t;
	triangles : (int * int * int * int) DynArray.t;
	bboxes    : (v3 * v3 * sceneobj list) DynArray.t;
    }
let scene_new () =
    {
	vertices  = DynArray.create ();
	tridata   = DynArray.init 1 (fun _ -> [0.0; 1.0; 0.0]); (* FIXME: make more general *)
	triangles = DynArray.create ();
	bboxes    = DynArray.create ();
    }

let scene_add_vertex : ('a,'b) scene -> (v3 * 'a) -> int =
fun scene v ->
    let i = DynArray.length scene.vertices in
    (DynArray.add scene.vertices v; i)

let scene_add_triangle : ('a,'b) scene -> (int * int * int) -> int =
fun scene (a,b,c) ->
    let i = DynArray.length scene.triangles in
    (DynArray.add scene.triangles (a,b,c,0); i)

let scene_add_bbox : ('a,'b) scene -> (v3*v3*sceneobj list) -> int =
fun scene bb ->
    let i = DynArray.length scene.bboxes in
    (DynArray.add scene.bboxes bb; i)

(*
 * FIXME: compatibility function
 *)
let scene_add_triangle_naive : ('a,'b) scene -> v3 -> v3 -> v3 -> unit =
fun scene a b c ->
    let ai = scene_add_vertex scene (a, ()) in
    let bi = scene_add_vertex scene (b, ()) in
    let ci = scene_add_vertex scene (c, ()) in
    let _ = scene_add_triangle scene (ai,bi,ci) in
    ()

let scene_load_from_file : string -> ('a,'b) scene =
fun scene_file_name ->
    let scene_file_handler = open_in scene_file_name in
    let scene = scene_new () in
    let scan_line () =
	try
	    Scanf.fscanf scene_file_handler "%f %f %f %f %f %f %f %f %f" (
		fun ax ay az bx by bz cx cy cz ->
	            scene_add_triangle_naive
			scene
			(v3_make ax ay az)
			(v3_make bx by bz)
			(v3_make cx cy cz);
		    true
	    )
	with End_of_file ->
		    false
in
    let rec scan_file () =
        if scan_line()
	then scan_file()
	else ()
in
    (
	scan_file ();
	scene
    )

(******************************************************)
(* Global parameters, settable from the command line  *)
(******************************************************)

let leaf_max     = ref 4
let polygon_file = ref "../scene/bunny2.dat"
let output_file  = ref "bunny-compiled.dat"

(******************************************)
(* CONSTRUCTING THE KD-TREE               *)
(******************************************)

(*
 * The idea is that:
 * bb_union null_bb x = x
 *)
let bb_union : (v3*v3) -> (v3*v3) -> (v3*v3)
= fun        ((amin,amax))
	     ((bmin,bmax)) ->
	     (
	        componentwise_min amin bmin,
		componentwise_max amax bmax
	     )
let null_bb : (v3*v3)
= (v3_make infinity infinity infinity,
   v3_make neg_infinity neg_infinity neg_infinity)

let compute_order_x objects =
    let get_min_x (_,(mn,_)) = mn.x
 in let order_x = Array.init (Array.length objects) (fun i->(get_min_x objects.(i),i))
 in let _ = Array.fast_sort (fun (av,_) (bv,_) -> compare av bv) order_x
 in Array.map (fun (_,i) -> i) order_x

let compute_order_y objects =
    let get_min_y (_,(mn,_)) = mn.y
 in let order_y = Array.init (Array.length objects) (fun i->(get_min_y objects.(i),i))
 in let _ = Array.fast_sort (fun (av,_) (bv,_) -> compare av bv) order_y
 in Array.map (fun (_,i) -> i) order_y

let compute_order_z objects =
    let get_min_z (_,(mn,_)) = mn.z
 in let order_z = Array.init (Array.length objects) (fun i->(get_min_z objects.(i),i))
 in let _ = Array.fast_sort (fun (av,_) (bv,_) -> compare av bv) order_z
 in Array.map (fun (_,i) -> i) order_z

let rec build_bb_tree_aux2 scene (bb_min,bb_max) obj_array order_x order_y order_z =
    if Array.length obj_array = 0
    then
	failwith "Semi-internal error: in build_bb_tree_aux(obj_array), obj_array has 0 size"
    else if Array.length obj_array = 1
    then
	let (tri_index,_) = obj_array.(0) in T(tri_index)
    else if Array.length obj_array <= (!leaf_max) (* default - 4 *)
    then    let objs = List.map (fun (tri_index,_) -> T(tri_index)) (Array.to_list obj_array)
	 in B (scene_add_bbox scene (bb_min,bb_max,objs))
    else    
    let get_bb (_,bb) = bb
 (* cost = surface area * number of objects inside
  * This cost function forces overbalancing
  * The actual cost is probably more like surface * log(number of objects inside)
  *)
 in let bb_cost (q0,q1) obj_cnt =
       let sz = q1 -& q0
    in ((sz.x*.sz.y) +. (sz.x*.sz.z) +. (sz.y*.sz.z)) *. (float_of_int obj_cnt)
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

 in let left_obj  = build_bb_tree_aux2 scene bb_left left_objects left_order_x left_order_y left_order_z
 in let right_obj = build_bb_tree_aux2 scene bb_right right_objects right_order_x right_order_y right_order_z

 (* [right_obj; left_obj] results in about 2% BB/ray intersection tests for the bunny scene
  * than [left_obj; right_obj].
  * Of course it doesn't "really" matter
  *)
 in B (scene_add_bbox scene (bb_min,bb_max,[right_obj; left_obj]))

let build_bb_tree_aux scene obj_array =
    let global_bb = Array.fold_left (fun bb0 (_,bb1) -> bb_union bb0 bb1) null_bb obj_array
 in let order_x = compute_order_x obj_array
 in let order_y = compute_order_y obj_array
 in let order_z = compute_order_z obj_array
 in build_bb_tree_aux2 scene global_bb obj_array order_x order_y order_z

let build_bb_tree scene =
    let bb_triangle tri_index =
	let (ai,bi,ci,_) = DynArray.get scene.triangles tri_index in
	let (a,_) = DynArray.get scene.vertices ai in
	let (b,_) = DynArray.get scene.vertices bi in
	let (c,_) = DynArray.get scene.vertices ci in
	(tri_index,
	 (
	    (v3_make (min3 a.x b.x c.x) (min3 a.y b.y c.y) (min3 a.z b.z c.z)),
	    (v3_make (max3 a.x b.x c.x) (max3 a.y b.y c.y) (max3 a.z b.z c.z))
	 )
	)
in
    let process_list = Array.init (DynArray.length scene.triangles) bb_triangle in
    build_bb_tree_aux scene process_list


(********************
 *                  *
 * OLD CODE FOLLOWS *
 *                  *
 ********************)

(******************************************)
(* LOADING TRIANGLES                      *)
(******************************************)

let bb1d : float -> float -> float -> (float * float)
= fun a b c ->
    if a < b
    then ((min a c), (max b c))
    else ((min b c), (max a c))
let triangle_bounding_box : v3 -> v3 -> v3 -> (v3*v3)
= fun a b c ->
    let (x0,x1) = bb1d a.x b.x c.x
 in let (y0,y1) = bb1d a.y b.y c.y
 in let (z0,z1) = bb1d a.z b.z c.z
 in ((v3_make x0 y0 z0),(v3_make x1 y1 z1))

(******************************************)
(* Scene marshalling                      *)
(******************************************)

(*
 * Trace unit expects the root_bb to be at 0 and be a bb
 * So insert a root bb if the root is a triangle
 *
 * FIXME: do it
 *)
let rec marshal_scene : ('a, 'b) scene ->
                        ('a -> int) ->
			('a -> float list) ->
			('b -> int) ->
			('b -> float list) ->
			sceneobj ->
			float array =
fun scene
    vertex_data_sz
    vertex_data_marshal
    triangle_data_sz
    triangle_data_marshal
    root_bb_index
    ->
    let tri_ofs     = Array.create (DynArray.length scene.triangles) 0 in
    let tridata_ofs = Array.create (DynArray.length scene.tridata) 0 in
    let vtx_ofs     = Array.create (DynArray.length scene.vertices) 0 in
    let bb_ofs      = Array.create (DynArray.length scene.bboxes) 0 in
    let bb_ord      = Array.create (DynArray.length scene.bboxes) 0 in
    let mem_end     = ref 0 in
    let bb_dst_i    = ref 0 in
    let rec compute_bb_ofs = function
	| T _ -> ()
	| B i ->
	(
	    let (mn,mx,objs) = DynArray.get scene.bboxes i in
	    let objsz = 7 + List.length objs in
	    bb_ord.(!bb_dst_i) <- i;
	    bb_ofs.(i)         <- !mem_end;
	    bb_dst_i    := !bb_dst_i + 1;
	    mem_end     := !mem_end + objsz;
	    List.iter compute_bb_ofs objs
	)
in
    let _ = compute_bb_ofs root_bb_index
in
    let _ = DynArray.iteri (fun i _ ->
	    let objsz = 4 in
	    tri_ofs.(i) <- !mem_end;
	    mem_end     := !mem_end + objsz
	  ) scene.triangles
in
    let _ = DynArray.iteri (fun i tri_data ->
	    let objsz = triangle_data_sz tri_data in
	    tridata_ofs.(i) <- !mem_end;
	    mem_end         := !mem_end + objsz
	  ) scene.tridata
in
    let _ = DynArray.iteri (fun i (_,vtx_data) ->
	    let objsz = 3 + vertex_data_sz vtx_data in
	    vtx_ofs.(i) <- !mem_end;
	    mem_end      := !mem_end + objsz
	  ) scene.vertices
in
    let result = Array.create !mem_end 0.0 in
    let _ = Array.iter (fun bb_i ->
	let (mn, mx, objs) = DynArray.get scene.bboxes bb_i in
	let addr = bb_ofs.(bb_i) in
	result.(addr)   <- mn.x;
	result.(addr+1) <- mn.y;
	result.(addr+2) <- mn.z;
	result.(addr+3) <- mx.x;
	result.(addr+4) <- mx.y;
	result.(addr+5) <- mx.z;
	Array.iteri (fun ofs -> function
	| T i -> result.(addr+6+ofs) <- float_of_int (4*tri_ofs.(i) + 1)
	| B i -> result.(addr+6+ofs) <- float_of_int (4*bb_ofs.(i))
	) (Array.of_list objs);
	result.(addr+6+List.length objs) <- 3.0;
    ) bb_ord
in
    let _ = Array.iteri (fun tri_i addr ->
	let (ai,bi,ci,tdi) = DynArray.get scene.triangles tri_i in
	result.(addr)   <- float_of_int (vtx_ofs.(ai));
	result.(addr+1) <- float_of_int (vtx_ofs.(bi));
	result.(addr+2) <- float_of_int (vtx_ofs.(ci));
	result.(addr+3) <- float_of_int (tridata_ofs.(tdi));
    ) tri_ofs
in
    let _ = Array.iteri (fun td_i addr ->
	let td = DynArray.get scene.tridata td_i in
	let tri_data = Array.of_list (triangle_data_marshal td) in
	Array.iteri (fun ofs v ->
	    result.(addr+ofs) <- v;
	) tri_data
    ) tridata_ofs
in
    let _ = Array.iteri (fun vtx_i addr ->
	let (v,vd) = DynArray.get scene.vertices vtx_i in
	result.(addr)   <- v.x;
	result.(addr+1) <- v.y;
	result.(addr+2) <- v.z;
	let vtx_data = Array.of_list (vertex_data_marshal vd) in
	Array.iteri (fun ofs v ->
	    result.(addr+3+ofs) <- v;
	) vtx_data
    ) vtx_ofs
in
    result

(******************************************)
(* MAIN                                   *)
(******************************************)

let save_scene filename marshalled_data = output_value (open_out filename) marshalled_data

let arg_spec = [
    ("-o", Arg.String (fun arg -> output_file := arg), "output file");
    ("-s", Arg.String (fun arg -> polygon_file := arg), "polygon file");
    ("-k", Arg.Int    (fun arg -> leaf_max := arg), "max objects in leaf nodes")
]
let parse_rest str = failwith (sprintf "Parse error: argument %s not understood" str)

let usage_msg = "./build_kdtree.opt -s <source> -o <target>"
let _ = Arg.parse arg_spec parse_rest usage_msg
;;

let scene : (unit, float list) scene = scene_load_from_file (!polygon_file) in
let root_bb = build_bb_tree scene in
let marshalled_data =
    marshal_scene
	scene
	(fun _ -> 0)
	(fun _ -> [])
	(fun x -> List.length x)
	(fun x -> x)
	root_bb
in
save_scene (!output_file) marshalled_data
