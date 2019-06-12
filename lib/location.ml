type t = {
  line   :int;
  column :int;
}
[@@deriving ord]

let string_of loc =
  Printf.sprintf "line: %d column: %d" loc.line loc.column

(* Most editors and compilers (that I use) count lines from 1 but columns from 0. *)
let zero = { line = 1; column = 0 }
