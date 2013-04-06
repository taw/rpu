type v3 = float * float * float

val ( +& ) : v3 -> v3 -> v3
val ( -& ) : v3 -> v3 -> v3
val ( *& ) : v3 -> v3 -> float
val ( *&& ) : v3 -> v3 -> v3
val ( *&* ) : v3 -> float -> v3
val ( /& ) : v3 -> float -> v3
val scnorm : v3 -> float
val v3norm : v3 -> v3

val max3 : 'a -> 'a -> 'a -> 'a
val min3 : 'a -> 'a -> 'a -> 'a
      