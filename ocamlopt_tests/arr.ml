type vec = float array

let get_x : vec -> float = function a -> a.(0)
let get_y : vec -> float = function a -> a.(1)
let get_z : vec -> float = function a -> a.(2)

let dotprod : vec -> vec -> float = fun a b ->
    get_x a *. get_x b +.
    get_y a *. get_y b +.
    get_z a *. get_z b
