open Xml_scene

let (scene : (v3, bool ref) scene) = input_value stdin

let pp_print_bool_ref state br = Format.pp_print_string state (string_of_bool (!br))

let ppf = Format.std_formatter
let _   = IoXML.xprint ppf "@[%a@]@." (xprint_scene xprint_v3 pp_print_bool_ref) scene
