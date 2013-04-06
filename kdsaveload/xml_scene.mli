type v3 = float * float * float
type ('a,'b) scene = Triangle of v3 * v3 * v3 * 'a | BBox of 'b * v3 * v3 * ('a,'b) scene list
