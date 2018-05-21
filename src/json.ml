type t = [
  | `Object of (string * t) list
  | `Bool of bool
  | `Number of float
  | `Array of t array
  | `Null
  | `String of string
]

external typeof : 'a -> string = "" [@@bs.val]

external isArray : 'a -> bool = "" [@@bs.scope "Array"] [@@bs.val]

external null : 'a = "" [@@bs.val]

let isNull : 'a -> bool = [%bs.raw fun v ->
  "return v === null;"
]

let mapObj : 'a Js.t -> ('a -> 'b) -> (string*'b) array = [%bs.raw fun obj fn ->
  "var ret = [];
   for (var idx in obj) {
     var v = fn(obj[idx]);
     ret.push([idx, v]);
   }
   return ret;"
]

let makeObj : (string*'a) array -> 'b = [%bs.raw fun a ->
  "var ret = {};
  for (var idx in a) {
    var data = a[idx];
    ret[data[0]] = data[1];
  }
  return ret;"
]

external parse : string -> 'a = "" [@@bs.scope "JSON"] [@@bs.val]

external stringify : string -> 'a = "" [@@bs.scope "JSON"] [@@bs.val]

let rec json_of_value v =
  match typeof v with
    | "object" when isArray v ->
        `Array (Array.map json_of_value (Obj.magic v))
    | "object" when isNull v ->
        `Null
    | "object" ->
        `Object (Array.to_list (mapObj (Obj.magic v) json_of_value))
    | "boolean" -> `Bool (Obj.magic v)
    | "number"  -> `Number (Obj.magic v)
    | "string"  -> `String (Obj.magic v)
    | _ -> assert false

let rec value_of_json = function
  | `Object v ->
      let v = List.map (fun (lbl,v) ->
        (lbl, value_of_json v)) v
      in
      makeObj (Array.of_list v)
  | `Bool b -> Obj.magic b
  | `Number v -> Obj.magic v
  | `Array a -> Obj.magic a
  | `Null -> null
  | `String s -> Obj.magic s

let parse s =
  json_of_value (parse s)

let stringify v =
  stringify (value_of_json v)

let from_js v =
  match json_of_value (Obj.magic v) with
    | `Object l -> l
    | _ -> assert false

 let to_js l =
   Obj.magic
     (value_of_json (`Object l))

