open Printf
open Scene
open Math

(******************************************)
(* AUX FUNCTIONS                          *)
(******************************************)

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
(* SCENE LOADING                          *)
(******************************************)


let scene_file  = ref "scene.scm"
let output_file = ref "img.pnm"
let shader_file = ref "depth.shr"
let height      = ref 512
let width       = ref 512
let trace_x	= ref None
let trace_y	= ref None

let arg_spec = [
    ("-o", Arg.String (fun arg -> output_file := arg), "output file");
    ("-s", Arg.String (fun arg -> scene_file := arg),  "scene file");
    ("-h", Arg.String (fun arg -> shader_file := arg), "shader file");
    ("-x", Arg.Int (fun arg -> width := arg),          "image width");
    ("-y", Arg.Int (fun arg -> height := arg),         "image height");
    ("-tx",Arg.Int (fun arg -> trace_x := Some arg),   "trace at one point, x");
    ("-ty",Arg.Int (fun arg -> trace_y := Some arg),   "trace at one point, y");
]
let parse_rest str = failwith (sprintf "Parse error: argument %s not understood" str)

let usage_msg = "./raytracer -h height -w width -s <source> -o <target>"
let _ = Arg.parse arg_spec parse_rest usage_msg

let scene = load_scene (!scene_file)
let output_file = open_out (!output_file)

let green = (0.0, 1.0, 0.0)
let black = (0.0, 0.0, 0.0)

(******************************************)
(* BB tree traversal                      *)
(******************************************)

(*
 * Algorithm from "An Efficient and Robust Ray-Box Intersection Algorithm" paper
 * by Amy Williams, Steve Barrus, R. Keith Morley, and Peter Shirley
 *
 * idir's sign is compared instead of dir's to behave correctly
 * with -0.0 (IEEE 753 negative zero)
 *
 * t0,t1 - minimum and maximum distance, typically +eps,+inf for primary rays
 *         and +eps,distance_to_light_source for shadow rays
 *)
let ray_bb_intersection_test ((ox,oy,oz) as orig) ((idx,idy,idz) as idir) (x0,y0,z0) (x1,y1,z1) (t0,t1) =
(* sg could be precomputed like idir *)
    let (sgx,sgy,sgz) = (idx < 0.0, idy < 0.0, idz < 0.0)
 in let (xa,xb) = if sgx then (x1,x0) else (x0,x1)
 in let tmin_x = (xa -. ox) *. idx
 in let tmax_x = (xb -. ox) *. idx
 in let (ya,yb) = if sgy then (y1,y0) else (y0,y1)
 in let tmin_y = (ya -. oy) *. idy
 in let tmax_y = (yb -. oy) *. idy
 in if tmin_x > tmax_y || tmin_y > tmax_x
    then false
    else
    let tmin_xy = max tmin_x tmin_y
 in let tmax_xy = min tmax_x tmax_y
 in let (za,zb) = if sgz then (z1,z0) else (z0,z1)
 in let tmin_z = (za -. oz) *. idz
 in let tmax_z = (zb -. oz) *. idz
 in if tmin_xy > tmax_z || tmin_z > tmax_xy
    then false
    else
    let tmin = max tmin_xy tmin_z
 in let tmax = min tmax_xy tmax_z
 in (tmin < t1) && (tmax > t0)

(******************************************)
(* MAIN PROGRAM                           *)
(******************************************)

(*  Y  Z
 *  ^ /
 *  |/
 *  --->X
 *)


(* From "Fast, Minimum Storage Ray/Triangle Intersection" paper
 * by Thomas Moller, Ben Trumbore
 *
 * ASSERTION (unverified) - it always returns hit distance >0
 *)
let eps = 1.0e-5
let ray_triangle_intersection_test orig dir obj =
    let (a, b, c, _) = obj
 in let e1 = b -& a
 in let e2 = c -& a
 in let p  = dir *&& e2
 in let det = e1 *& p
 in if det > ~-.eps && det < eps
    then None
    else
    let idet = 1.0 /. det
 in let t = orig -& a
 in let u = (t *& p) *. idet
 in if u < 0.0 || u > 1.0
    then None
    else
    let q = t *&& e1
 in let v = (dir *& q) *. idet
 in if v < 0.0 || (u+.v) > 1.0
    then None
    else
    let hit_dist = (e2 *& q) *. idet
 in (Some (hit_dist, obj))

let debug_info = [| 0; 0; 0; 0 |]
let debug i    = (debug_info.(i) <- debug_info.(i) + 1)
let bb_stat_predict_hit bb_stat_obj   = bb_stat_obj := false
let bb_stat_predict_miss bb_stat_obj  = bb_stat_obj := true
let bb_stat_predict_miss_p bb_stat_obj = !bb_stat_obj

let shot_ray orig dir =
    let best_hit = ref None
 (* duplicates best_hit for no good reason *)
 in let closest_hit_so_far = ref infinity
 in let set_hit hit = match !best_hit with
    | None -> best_hit := Some hit
    | Some (best_dist,_) ->
	let (hit_dist,_) = hit
     in if hit_dist < best_dist 
	then (best_hit := Some hit; closest_hit_so_far := hit_dist)
	else ()
 in let idir = let (dx,dy,dz) = dir in (1.0/.dx, 1.0/.dy, 1.0/.dz)
 in let rec traverse_bbtree = function
    | BBox(bb_stat_obj,mn,mx,objs) ->
	if bb_stat_predict_miss_p bb_stat_obj
	then
	    if ray_bb_intersection_test orig idir mn mx (0.0,!closest_hit_so_far)
	    then (
		debug 0;
		bb_stat_predict_hit bb_stat_obj;
		list_eager_for_any traverse_bbtree objs;
		)
	    else
		(debug 1; bb_stat_predict_miss bb_stat_obj; false)
	else (* It will probably hit, so let's just assume it did *)
	    (
		if list_eager_for_any traverse_bbtree objs;
		then (debug 2; bb_stat_predict_hit bb_stat_obj; true)
		(* Correct the prediction *)
		else (debug 3; bb_stat_predict_miss bb_stat_obj; false)
	    )
    | Triangle(a,b,c,data) ->
	(match ray_triangle_intersection_test orig dir (a,b,c,data) with
	| None -> false
	| Some hit -> (set_hit hit; true)
	)
 in let _ = traverse_bbtree scene
 in !best_hit

let bg_color = (0.0, 0.0, 0.0)
let light = (-0.2, 0.5, 0.0)
let obj_color = green
let pixel_conversion x =
    let a = int_of_float (0.5 +. 255.0 *. x)
 in if a < 0
    then Char.chr 0
    else if a > 255
    then Char.chr 255
    else Char.chr a
let pixel (r,g,b) = fprintf output_file "%c%c%c" (pixel_conversion r) (pixel_conversion g) (pixel_conversion b)

let hit_result vm orig dir = function
  | None           -> pixel bg_color
  | Some (hit_dist,(a,b,c,data)) ->
  (* u,v are not passed the way they should 
   * The reason we even pass u,v,hp is because they have already been computer
   * but they're recomputed here, and that's stupid
   *)
(* Our own Phong calculations*)    
(*       let hit = orig +& (dir *&* hit_dist)
   in let lv = v3norm (light -& hit)
   in let sn_undir = v3norm ((b -& a) *&& (c -& a))
   in let sn = if (sn_undir *& dir) < 0.0
	       then sn_undir
	       else sn_undir *&* -1.0
   in let i  = max 0.0 (lv *& sn)
   it let i2 = 0.4 +. 0.6 *. i
   in let px = data *&* i2
*)
      let fc (x,y,z) = [|x;y;z;1.0|]
   in let hp = orig +& (dir *&* hit_dist)
   in let px = (Vm.shade_pixel vm 0.0 0.0 hit_dist (fc orig) (fc dir)
	      (fc hp) (fc a) (fc b) (fc c) (fc data) (fc light))
   in pixel px

(*
 * I have no idea what kind of projection it is
 *)
let render_scene orig dir0 right up xsz ysz =
   let vm = Vm.vm_new (!shader_file)
in let trace_ray y x =
    let xofs = right *&* (-1.0 +. 2.0 *. (float_of_int x) /. (float_of_int (xsz - 1)))
 in let yofs = up    *&* ( 1.0 -. 2.0 *. (float_of_int y) /. (float_of_int (ysz - 1)))
 in let dir  = v3norm (dir0 +& xofs +& yofs)
 in (dir, shot_ray orig dir)
in
match (!trace_x,!trace_y) with
| (None, None) ->
(* Normal tracing *)
    begin
     fprintf output_file "P6\n%d %d\n255\n" xsz ysz;
     for y = 0 to (ysz-1) do
	for x = 0 to (xsz-1) do
	    let dir,hit = (trace_ray y x)
	    in hit_result vm orig dir hit
	done;
     done
   end
| (Some tx, Some ty) -> 
    let (dir, hit) = trace_ray ty tx
 in (
    printf "Tracing ray at (%d,%d)\n" tx ty;
    match hit with
    | None -> printf "Miss\n"
    | Some(hit_dist,(a,b,c,data)) -> (
	printf "Hit a triangle at distance %g\n" hit_dist;
	let (ax,ay,az)=a in printf "A = (%g,%g,%g)\n" ax ay az;
	let (bx,by,bz)=b in printf "B = (%g,%g,%g)\n" bx by bz;
	let (cx,cy,cz)=c in printf "C = (%g,%g,%g)\n" cx cy cz;
	let fc (x,y,z) = [|x;y;z;1.0|]
	in let hp = orig +& (dir *&* hit_dist)
	in let (r,g,b) = Vm.shade_pixel_trace vm 0.0 0.0 hit_dist (fc orig) (fc dir)
					 (fc hp) (fc a) (fc b) (fc c) (fc data) (fc light)
	in printf "Pixel = (%g,%g,%g)\n" r g b
    )
)
| (_,_) -> failwith "In single pixel trace mode you must specify both of its coordinates"

let _ = render_scene (0.0, 0.0, 0.0) (0.0, 0.0, 1.0) (1.0, 0.0, 0.0) (0.0, 1.0, 0.0) (!width) (!height)
