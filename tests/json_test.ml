open Tape

let () =
  test "Parsing" (fun t ->
    let obj =
      `Object ["foo", `Number 123.4]
    in

    t.plan 5;
    t.ok (obj = Json.parse "{\"foo\":123.4}");
    t.ok (obj = Json.parse (Json.stringify obj));
    t.ok (`String "aabbcc" = Json.parse "\"aabbcc\"");
    t.ok ("\"aabbcc\"" == Json.stringify (`String "aabbcc"));
    t.ok (`Array [|`Number 1.;`String "foo";`Null|] = Json.parse "[1,\"foo\",null]"))
