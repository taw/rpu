open Math
type ('a,'b) scene = Triangle of v3 * v3 * v3 * 'a | BBox of 'b * v3 * v3 * ('a,'b) scene list
val bb_union : (v3*v3) -> (v3*v3) -> (v3*v3)
val null_bb : (v3*v3)

val save_scene : string -> ('a,'b) scene -> unit
val load_scene : string -> ('a,'b) scene
