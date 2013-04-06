class vec =
    fun a b c ->
object(self)
    val x : float = a
    val y : float = b
    val z : float = c
    method get_x = x
    method get_y = y
    method get_z = z
    method dotprod : vec->float = fun b ->
	(self#get_x *. b#get_x) +.
	(self#get_y *. b#get_y) +.
        (self#get_z *. b#get_z)
end
