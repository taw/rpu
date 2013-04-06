type vec = float * float * float

let get_x : vec -> float = function (x,_,_) -> x
let get_y : vec -> float = function (_,y,_) -> y
let get_z : vec -> float = function (_,_,z) -> z

let dotprod : vec -> vec -> float = fun a b ->
    get_x a *. get_x b +.
    get_y a *. get_y b +.
    get_z a *. get_z b
