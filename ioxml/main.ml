open Foo

let t1 = Triangle({x=0.0; y=0.0; z=1.0}, {x= 1.0; y=0.0; z=1.0}, {x=0.0; y= 1.0; z=1.0})
let t2 = Triangle({x=0.0; y=0.0; z=1.0}, {x= 1.0; y=0.0; z=1.0}, {x=0.0; y=(-1.0); z=1.0})
let t3 = Triangle({x=0.0; y=0.0; z=1.0}, {x=(-1.0); y=0.0; z=1.0}, {x=0.0; y= 1.0; z=1.0})
let t4 = Triangle({x=0.0; y=0.0; z=1.0}, {x=(-1.0); y=0.0; z=1.0}, {x=0.0; y=(-1.0); z=1.0})
let scene = BBox({x=(-1.0); y=(-1.0); z=1.0}, {x=1.0; y=1.0; z=1.5}, [t1; t2; t3; t4])

let ppf = Format.std_formatter
let _   = IoXML.xprint ppf "@[%a@]@." xprint_scene scene
