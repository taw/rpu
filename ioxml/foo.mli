type v3 = {x: float ; y: float; z:float}
type scene = Triangle of v3*v3*v3 | BBox of v3*v3*scene list
