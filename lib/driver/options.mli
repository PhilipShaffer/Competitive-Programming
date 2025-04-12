
(** Command line options *)
type t = {
  filename : string;
  output_name : string;
  show_tokens : bool;
  target : Compiler.target;
}

(** Parse command line arguments *)
val parse : unit -> t

(** Get usage information *)
val usage : string 