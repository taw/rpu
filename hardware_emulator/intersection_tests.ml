open Math3

(*
 * Algorithm from "An Efficient and Robust Ray-Box Intersection Algorithm" paper
 * by Amy Williams, Steve Barrus, R. Keith Morley, and Peter Shirley
 *
 * idir's sign is compared instead of dir's to behave correctly
 * with -0.0 (IEEE 753 negative zero)
 *
 * API:
 * orig - ray origin
 * idir - vector of ray component's inverses (1/x, 1/y, 1/z)
 * x0,y0,z0,x1,y1,z1 - bounding box (x0 <= x1 etc.)
 * t0,t1 - minimum and maximum distance, typically +eps,+inf for primary rays
 *         and +eps,distance_to_light_source for shadow rays (t0 <= t1)
 *
 * returns true/false
 *)

let ray_bb_intersection_test : point -> vector -> point -> point -> (float * float) -> bool
= fun orig idir mn mx (t0,t1) ->
(* sg could be precomputed like idir *)
    let (sgx,sgy,sgz) = (idir.x < 0.0, idir.y < 0.0, idir.z < 0.0) in
    let (xa,xb) = if sgx then (mx.x,mn.x) else (mn.x, mx.x) in
    let tmin_x = (xa -. orig.x) *. idir.x in
    let tmax_x = (xb -. orig.x) *. idir.x in
    let (ya,yb) = if sgy then (mx.y,mn.y) else (mn.y,mx.y) in
    let tmin_y = (ya -. orig.y) *. idir.y in
    let tmax_y = (yb -. orig.y) *. idir.y in
    if tmin_x > tmax_y || tmin_y > tmax_x
    then
	false
    else
        let tmin_xy = max tmin_x tmin_y in
        let tmax_xy = min tmax_x tmax_y in
        let (za,zb) = if sgz then (mx.z,mn.z) else (mn.z,mx.z) in
        let tmin_z = (za -. orig.z) *. idir.z in
        let tmax_z = (zb -. orig.z) *. idir.z in
        if tmin_xy > tmax_z || tmin_z > tmax_xy
        then
	    false
	else
	    let tmin = max tmin_xy tmin_z in
	    let tmax = min tmax_xy tmax_z in
	    (tmin < t1) && (tmax > t0)

(* Algorithm from "Fast, Minimum Storage Ray/Triangle Intersection" paper
 * by Thomas Moller, Ben Trumbore
 *
 * FIXME: ASSERTION (unverified) - it always returns hit distance >0
 *
 * eps is used to avoid processing triangles almost parallel to the ray
 * and numerical problems caused by them. This can cause some problems
 * when a ray actually hits one.
 *
 * orig    - ray origit
 * dir     - ray direction vector
 * a       - triangle vertex A
 * b       - triangle vertex B
 * c       - triangle vertex C
 *
 * returns Some(hit_distance,u,v)/None
 *)
let eps = 1.0e-5
let ray_triangle_intersection_test : point -> vector -> (point * point * point) -> (float * float * float) option
= fun orig dir (a,b,c) ->
    let e1 = b -& a in
    let e2 = c -& a in
    let p  = dir *^& e2 in
    let det = e1 *.& p in
    if det > ~-.eps && det < eps
    then
	None
    else
	let idet = 1.0 /. det in
	let t = orig -& a in
	let u = (t *.& p) *. idet in
	if u < 0.0 || u > 1.0
	then
	    None
	else
	    let q = t *^& e1 in
	    let v = (dir *.& q) *. idet in
	    if v < 0.0 || (u+.v) > 1.0
	    then
		None
	    else
		let hit_dist = (e2 *.& q) *. idet in
		Some (hit_dist, u, v)
