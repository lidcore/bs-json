type t = [
  | `Object of (string * t) list
  | `Bool of bool
  | `Number of float
  | `Array of t array
  | `Null
  | `String of string
]

val parse     : string -> t
val stringify : t -> string
val from_js   : 'a Js.t -> (string * t) list
val to_js     : (string * t) list -> 'a Js.t
