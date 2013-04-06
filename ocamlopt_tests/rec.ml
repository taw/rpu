type vec = { x : float; y : float; z : float }

let get_x : vec -> float = function a -> a.x
let get_y : vec -> float = function a -> a.y
let get_z : vec -> float = function a -> a.z
let geti : vec -> int -> float = fun v -> function
    | 0 -> v.x
    | 1 -> v.y
    | 2 -> v.z
    | _ -> failwith "Index out of range"
let unsafe_geti : vec -> int -> float = fun v i ->
    Obj.magic (Obj.field (Obj.repr v) i)

let dotprod : vec -> vec -> float = fun a b ->
    get_x a *. get_x b +.
    get_y a *. get_y b +.
    get_z a *. get_z b

let dotprod2 : vec -> vec -> float = fun a b ->
    unsafe_geti a 0 *. unsafe_geti b 0 +.
    unsafe_geti a 1 *. unsafe_geti b 1 +.
    unsafe_geti a 2 *. unsafe_geti b 0
