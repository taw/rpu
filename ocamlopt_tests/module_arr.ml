module type VECTOR3 =
sig
    type t
    val get_x : t -> float
    val get_y : t -> float
    val get_z : t -> float
    val geti  : t -> int -> float
    val unsafe_geti : t -> int -> float
    val make : float -> float -> float -> t
end
;;
module Vector3 : VECTOR3 =
struct
    type t = float array
    let get_x v = Array.unsafe_get v 0
    let get_y v = Array.unsafe_get v 1
    let get_z v = Array.unsafe_get v 2
    let geti  v i = Array.get v i
    let unsafe_geti v i = Array.unsafe_get v i
    let make x y z = [| x; y; z |]
end
;;

type vec  = Vector3.t
let get_x = Vector3.get_x
let get_y = Vector3.get_y
let get_z = Vector3.get_z

let dotprod : vec -> vec -> float = fun a b ->
    get_x a *. get_x b +.
    get_y a *. get_y b +.
    get_z a *. get_z b
