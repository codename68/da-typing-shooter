type t

exception Game_finished of int

val init: Sdl.Render.t -> t

val quit: t -> unit

val handle_events: t -> Sdl.Event.t -> unit

val think: t -> float -> unit

val draw: t -> unit
