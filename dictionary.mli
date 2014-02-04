open CamomileLibrary

type t

val init: unit -> t
val get_random_word: t -> int -> UChar.t list -> UTF8.t option
