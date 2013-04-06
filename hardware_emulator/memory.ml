type memory = float DynArray.t
type addr = int

let fetch_float : memory -> addr -> float =
fun mem addr ->
    if (addr >= 0 && addr < DynArray.length mem)
    then DynArray.unsafe_get mem addr (* no need to double-check *)
    else 0.0                          (* better this than raising silly exceptions
                                         LOAD may load as many as 3 words after allocated memory *)

let fetch_v3    : memory -> addr -> Math3.v3 =
fun mem addr ->
    let x = fetch_float mem addr     in
    let y = fetch_float mem (addr+1) in
    let z = fetch_float mem (addr+2) in
    Math3.v3_make x y z

let fetch_cell4 : (float -> float -> float -> float -> 'a) -> memory -> addr -> 'a =
fun costructor mem addr ->
    let x = fetch_float mem addr     in
    let y = fetch_float mem (addr+1) in
    let z = fetch_float mem (addr+2) in
    let w = fetch_float mem (addr+3) in
    costructor x y z w

let store_float : memory -> addr -> float -> unit =
fun mem addr v ->
    DynArray.set mem addr v

let store_cell4 : memory -> addr -> float -> float -> float -> float -> unit =
fun mem addr x y z w ->
(
    store_float mem (addr)   x;
    store_float mem (addr+1) y;
    store_float mem (addr+2) z;
    store_float mem (addr+3) w;
)
