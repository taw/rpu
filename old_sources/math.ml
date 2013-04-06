type v3 = float * float * float

(******************************************)
(* 3-VECTOR OPERATIONS                    *)
(******************************************)
let (+&)    (ax,ay,az) (bx,by,bz) = (ax+.bx, ay+.by, az+.bz)
let (-&)    (ax,ay,az) (bx,by,bz) = (ax-.bx, ay-.by, az-.bz)
let ( *& )  (ax,ay,az) (bx,by,bz) = (ax*.bx +. ay*.by +. az*.bz)
let ( *&& ) (ax,ay,az) (bx,by,bz) = ((ay*.bz)-.(az*.by), (az*.bx)-.(ax*.bz), (ax*.by)-.(ay*.bx))
let scnorm a = sqrt (a *& a)
let ( *&* ) (ax,ay,az) b = (ax*.b, ay*.b, az*.b)
let ( /& )  (ax,ay,az) b = (ax/.b, ay/.b, az/.b)
(* norm(0) = 0 ? NaN ? undefined ? *)
let v3norm a = a /& (scnorm a)

let max3 a b c = max (max a b) c
let min3 a b c = min (min a b) c
