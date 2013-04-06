(*
 * Aliases point and vector are just for nicer explicit types
 *)
type v3 = { x : float; y : float; z : float }
type point = v3
type vector = v3

(********************************************************************)
(* 3-VECTOR OPERATIONS                                              *)
(*                                                                  *)
(* All have & suffix                                                *)
(* The first character determines operator precedence               *)
(* *.& - dot product                                                *)
(* *^& - scalar product                                             *)
(* *&  - multiplication by a scalar                                 *)
(********************************************************************)
let ( +&  ) a b = {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
let ( -&  ) a b = {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
let ( *&  ) a b = {x = a.x *. b;   y = a.y *. b;   z = a.z *. b   }
let ( /&  ) a b = {x = a.x /. b;   y = a.y /. b;   z = a.z /. b   }
let ( *.& ) a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)
let ( *^& ) a b = {
    x = (a.y *. b.z) -. (a.z *. b.y);
    y = (a.z *. b.x) -. (a.x *. b.z);
    z = (a.x *. b.y) -. (a.y *. b.x);
}

(* TODO: what is norm(0) supposed to be - 0, NaN, undefined ? *)
let norm a = sqrt (a *.& a)

let normalize a = a /& (norm a)
let componentwise_apply f v = { x = f v.x; y = f v.y; z = f v.z; }
let componentwise_inverse a = { x = 1.0/.a.x; y = 1.0/.a.y; z = 1.0/.a.z }

let max3 a b c = max (max a b) c
let min3 a b c = min (min a b) c

let componentwise_min a b = { x = min a.x b.x;
                              y = min a.y b.y;
			      z = min a.z b.z;
			    }
let componentwise_max a b = { x = max a.x b.x;
                              y = max a.y b.y;
			      z = max a.z b.z;
			    }
let v3_make x y z = {x = x; y = y; z = z}
