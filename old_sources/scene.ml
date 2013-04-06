open Math
type ('a,'b) scene = Triangle of v3 * v3 * v3 * 'a | BBox of 'b * v3 * v3 * ('a,'b) scene list
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

let save_scene filename scene = output_value (open_out filename) scene
let load_scene filename       = input_value  (open_in  filename)
