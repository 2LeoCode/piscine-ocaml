module type FIXED = sig
  type t

  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool
  val eqs : t -> t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONAL_BITS = sig
  val bits : int
end

module Make (T : FRACTIONAL_BITS) : FIXED = struct
  let () =
    if T.bits < 0 || T.bits > 64 then
      invalid_arg "FRACTIONAL_BITS.bits must be in the interval [0; 64]"

  type t = int

  let factor = 1 lsl T.bits
  let of_float f = int_of_float (f *. float_of_int factor)
  let of_int i = i * factor
  let to_float v = float_of_int v /. float_of_int factor
  let to_int v = v / factor
  let to_string v = string_of_float (to_float v)
  let zero = 0
  let one = of_int 1
  let succ v = v + one
  let pred v = v - one
  let min = min
  let max = max
  let gth = ( > )
  let lth = ( < )
  let gte = ( >= )
  let lte = ( <= )
  let eqp = ( == )
  let eqs = ( = )
  let add = ( + )
  let sub = ( - )
  let mul = ( * )
  let div = ( / )

  let rec foreach first last un =
    if first <= last then (
      un first;
      (foreach [@tailcall]) (first + 1) last un)
end

module Fixed4 : FIXED = Make (struct
  let bits = 4
end)

module Fixed8 : FIXED = Make (struct
  let bits = 8
end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach Fixed4.zero Fixed4.one (fun f ->
      print_endline (Fixed4.to_string f))
