open CamomileLibrary

exception Game_finished of int

type enemy = {
  id: int;
  mutable x: float;
  y: int;
  text: string;
  letter: UChar.t;
  text_texture: Sdl.Texture.t;
  w: int;
  h: int;
  draw: game -> enemy -> unit;
  think: game -> enemy -> float -> unit;
  on_kill: game -> enemy -> unit;
  on_miss: game -> enemy -> unit;
  on_touch: game -> enemy -> unit;
}

and game = {
  renderer: Sdl.Render.t;
  font: Sdlttf.font;
  dict: Dictionary.t;
  textures: (string * Sdl.Texture.t) list;
  mutable current_enemy: enemy option;
  mutable enemies: enemy list;
  mutable time: float;
  mutable next_wave: float;
  mutable life: int;
  mutable current_match: int;
  mutable score: int;
  mutable time_since_last_shot: float;
  mutable last_shot_missed: bool;
}

let id_ctr = ref 0

let font_file = "/usr/share/fonts/truetype/didot/GFSDidot.otf"

let load_image renderer file =
  let t = Sdlimage.load_png_rw (Sdlrwops.from_input (`Filename file)) in
  let texture =
    Sdltexture.create_from_surface renderer t
  in
  Sdl.Surface.free t;
  texture

let create_text_texture game text =
  let t = Sdlttf.(render_utf8_blended game.font text {r = 255; g = 255; b = 255; a = 255;}) in
  let texture =
    Sdltexture.create_from_surface game.renderer t
  in
  Sdl.Surface.free t;
  texture

let load_textures renderer =
  [
    "ground", load_image renderer "ground.png";
    "wall", load_image renderer "wall.png";
    "goblin", load_image renderer "goblin.png";
    "orc", load_image renderer "orc.png";
    "axe", load_image renderer "axe.png";
    "ghoul", load_image renderer "ghoul.png";
  ]

let get_texture game t =
  try
    List.assoc t game.textures
  with
  | Not_found ->
    let fail_message = Printf.sprintf "texture %s not registered" t in
    failwith fail_message

let get_text game size =
  let avoid = List.map (fun e -> e.letter) game.enemies in
  Dictionary.get_random_word game.dict size avoid

let max_enemies = 20
let lane_size = 20

let create_enemy
    game
    ?lane
    ~difficulty
    ~draw
    ~think
    ?(on_miss = fun _ _ -> ())
    ?(on_touch = fun _ _ -> ())
    ?(on_kill = fun _ _ -> ())
    ?(x = 400.)
    () =
  let lane =
    match lane with
    | None ->
      let lanes = Array.create max_enemies false in
      List.iter (fun e -> lanes.(e.y) <- true) game.enemies;
      let l = ref [] in
      Array.iteri (fun i b -> if not b then l := i :: !l) lanes;
      if !l = [] then
        None
      else
        Some (List.nth !l (Random.int (List.length !l)))
    | x -> x
  in
  let text = get_text game difficulty in
  match lane, text with
  | None, _ ->
    Printf.eprintf "Could not spawn: no lane available\n%!";
    None
  | _, None ->
    Printf.eprintf "Could not spawn: no word available\n%!";
    None
  | Some y, Some text ->
    let id = !id_ctr in
    incr id_ctr;
    let w, h = Sdlttf.size_utf8 game.font text in
    Some {
      id;
      x;
      y;
      text;
      text_texture = create_text_texture game text;
      letter = UTF8.get text 0;
      w;
      h;
      draw;
      think;
      on_miss;
      on_touch;
      on_kill;
    }

let draw_texture_enemy texture game enemy =
  let dst_rect = {
    Sdlrect.x = int_of_float enemy.x;
    Sdlrect.y = enemy.y * lane_size;
    Sdlrect.w = 16;
    Sdlrect.h = 16;
  }
  in
  Sdl.Render.copy game.renderer ~texture ~dst_rect ()

let spawn_wave game wave =
  List.iter
    (fun f ->
       match f game with
       | None -> ()
       | Some g -> game.enemies <- g :: game.enemies)
    wave

let move speed game enemy delta =
  enemy.x <- enemy.x -. delta *. speed

let repel m game enemy =
  enemy.x <- enemy.x +. m

let goblin game =
  create_enemy
    game
    ~difficulty: (Random.int 4 + 3)
    ~draw: (draw_texture_enemy (get_texture game "goblin"))
    ~think: (move 40.)
    ~on_miss: (repel (-10.))
    ~on_touch: (repel (5.))
    ()

let rec ghoul ?lane ?x game =
  create_enemy
    game
    ~difficulty: (Random.int 3 + 10)
    ~draw: (draw_texture_enemy (get_texture game "ghoul"))
    ~think: (move 20.)
    ~on_miss: (repel (-10.))
    ~on_touch: (repel (5.))
    ~on_kill:
      (fun game enemy ->
         if Random.int 4 = 0 then
           spawn_wave game [ghoul ~lane: enemy.y ~x: enemy.x])
    ?lane
    ?x
    ()

let axe ?lane ?x game =
  create_enemy
    game
    ~difficulty: 1
    ~draw: (draw_texture_enemy (get_texture game "axe"))
    ~think: (move 100.)
    ?lane
    ?x
    ()

let orc_axe_spawn_time = 2.
let orc_stop_pos = 300.
let orc_speed = 30.

let orc game =
  let spawn_axe = ref 0. in
  create_enemy
    game
    ~difficulty: (Random.int 3 + 7)
    ~draw:
      (fun g e ->
         draw_texture_enemy (get_texture game "orc") g e;
         let x, y = int_of_float e.x, e.y * lane_size in
         let p = int_of_float (16. *. !spawn_axe /. orc_axe_spawn_time) in
         Sdl.Render.set_draw_color g.renderer (255, 0, 0) 255;
         Sdl.Render.draw_line g.renderer ((x, y + 16), (x + p,  y + 16));
         Sdl.Render.set_draw_color g.renderer (0, 0, 0) 255)
    ~think:
      (fun g e d ->
         if e.x > orc_stop_pos then
           move orc_speed g e d
         else
           if !spawn_axe > orc_axe_spawn_time then
             begin
               spawn_axe := 0.;
               spawn_wave game [axe ~lane: e.y ~x: (e.x -. 16.)]
             end
           else
             spawn_axe := !spawn_axe +. d)
    ~on_touch: (fun _ _ -> spawn_axe := 0.)
    ()

let remove_enemy game enemy =
  begin
    match game.current_enemy with
    | None -> ()
    | Some e ->
      if e.id = enemy.id then
        game.current_enemy <- None
  end;
  Sdl.Texture.destroy enemy.text_texture;
  game.enemies <-
    List.filter (fun e -> e.id <> enemy.id) game.enemies

let spawn_delay t = Random.float 3. +. 6.

let waves =
  [
    0, [ goblin; ];
    1, [ goblin; goblin; ];
    2, [ goblin; goblin; goblin; ];
    2, [ ghoul; ];
    3, [ orc; orc; ];
    3, [ orc; goblin; goblin; ];
    4, [ goblin; goblin; goblin; goblin; goblin; ];
    4, [ axe; axe; axe; axe; axe; ];
    5, [ orc; orc; orc; ];
    6, [ ghoul; ghoul; ];
    6, [ axe; axe; axe; axe; axe; axe; axe; axe; ];
    7, [ ghoul; ghoul; ghoul; ];
    8, [ goblin; goblin; goblin; goblin; goblin; goblin; goblin; goblin; ];
    9, [ ghoul; ghoul; ghoul; ghoul; ];
    9, [ orc; orc; orc; orc; ];
    10, [ goblin; goblin; goblin; orc; orc; ghoul; ghoul; ghoul; ];
  ]

let difficulty t = int_of_float (t /. 10.)

let rec wave t =
  let r = List.nth waves (Random.int (List.length waves)) in
  if fst r <= difficulty t then
    snd r
  else
    wave t

let spawn game delta =
  (* for now, spawn a new enemy every [spawn_delay] *)
  if game.time +. delta > game.next_wave then
    begin
      game.next_wave <- game.next_wave +. spawn_delay game.time;
      spawn_wave game (wave game.time)
    end

let think game delta =
  List.iter (fun enemy -> enemy.think game enemy delta) game.enemies;
  
  let to_remove = ref [] in
  List.iter
    (fun enemy ->
       if enemy.x <= 0. then to_remove := enemy :: !to_remove)
    game.enemies;
  List.iter
    (fun enemy ->
       remove_enemy game enemy;
       game.life <- game.life - 1)
    !to_remove;

  if game.life <= 0 then
    raise (Game_finished game.score);
  spawn game delta;
  game.time <- game.time +. delta;
  game.time_since_last_shot <- game.time_since_last_shot +. delta

let rec biggest_prefix s1 i1 s2 i2 n =
  if i1 >= String.length s1 || i2 >= String.length s2 || UTF8.look s1 i1 <> UTF8.look s2 i2 then
    n
  else
    let size = UTF8.next s1 i1 - i1 in
    biggest_prefix s1 (i1 + size) s2 (i2 + size) (n + size)

let on_add game p =
  let e = match game.current_enemy with
    | Some e -> e
    | None -> failwith "on_add"
  in
  game.time_since_last_shot <- 0.;
  if game.current_match = String.length e.text then
    begin
      game.last_shot_missed <- false;
      e.on_kill game e;
      remove_enemy game e;
      game.score <- game.score + (UTF8.length e.text);
      game.current_enemy <- None
    end
  else if p = 0 then
    begin
      game.last_shot_missed <- true;
      e.on_miss game e
    end
  else
    begin
      game.last_shot_missed <- false;
      e.on_touch game e
    end

let on_input game text =
  if text <> "" then
    match game.current_enemy with
    | None ->
      let c = UTF8.get text 0 in
      let p = List.filter (fun e -> e.letter = c) game.enemies in
      if p <> [] then
        begin
          let e = List.hd p in
          game.current_enemy <- Some e;
          game.current_match <-
            biggest_prefix text 0 e.text 0 0;
          on_add game game.current_match
        end
    | Some e ->
      let p = biggest_prefix text 0 e.text game.current_match 0 in
      game.current_match <- game.current_match + p;
      on_add game p
      
let init renderer =
  Sdl.Keyboard.start_text_input ();
  Sdl.Keyboard.set_text_input_rect (Sdlrect.({ x = 0; y = 0; w = 500; h = 500; }));
  let font = Sdlttf.open_font font_file 16 in
  let dict = Dictionary.init () in
  {
    renderer;
    font;
    enemies = [];
    current_enemy = None;
    time = 0.;
    next_wave = 0.;
    life = 10;
    textures = load_textures renderer;
    current_match = 0;
    dict;
    score = 0;
    time_since_last_shot = 0.;
    last_shot_missed = false;
  }

let quit game =
  Sdl.Keyboard.stop_text_input ();
  Sdlttf.close_font game.font

let draw_enemy game enemy =
  enemy.draw game enemy;
  let dst_rect = {
      Sdlrect.x = int_of_float enemy.x + 16;
      Sdlrect.y = enemy.y * lane_size;
      Sdlrect.w = enemy.w;
      Sdlrect.h = enemy.h;
    }
  in
  Sdl.Render.fill_rect game.renderer dst_rect;
  Sdl.Render.copy game.renderer ~texture: enemy.text_texture ~dst_rect ()

let shot_draw_threshold = 0.1    

let draw game =
  (* Draw background *)
  for j = 0 to 500 / 16 do
    let dst_rect = {
      Sdlrect.x = 0;
      Sdlrect.y = j * 16;
      Sdlrect.w = 16;
      Sdlrect.h = 16;
    }
    in
    Sdl.Render.copy game.renderer
      ~texture: (get_texture game "wall")
      ~dst_rect ()
  done;

  for i = 1 to 500 / 16 do
    for j = 0 to 500 / 16 do
      let dst_rect = {
        Sdlrect.x = i * 16;
        Sdlrect.y = j * 16;
        Sdlrect.w = 16;
        Sdlrect.h = 16;
      }
      in
      Sdl.Render.copy game.renderer
        ~texture: (get_texture game "ground")
        ~dst_rect ()
    done
  done;

  (* Draw enemies *)
  List.iter (draw_enemy game) game.enemies;
  begin
    match game.current_enemy with
    | None -> ()
    | Some e ->
      let w, h =
        Sdlttf.size_utf8 game.font (String.sub e.text 0 game.current_match)
      in
      let x, y = int_of_float e.x, e.y * lane_size in
      Sdl.Render.set_draw_color game.renderer (255, 0, 0) 255;
      Sdl.Render.draw_line game.renderer
        ((x + 16, y + e.h), (x + 16 + w, y + e.h));
      Sdl.Render.draw_line game.renderer
        ((x + 16 + w, y), (x + 16 + w, y + e.h));
      (* Draw line to current enemy *)

      if game.time_since_last_shot < shot_draw_threshold then
        if game.last_shot_missed then
          Sdl.Render.set_draw_color game.renderer (255, 0, 0) 255
        else
          Sdl.Render.set_draw_color game.renderer (0, 0, 255) 255
      else
        Sdl.Render.set_draw_color game.renderer (128, 128, 128) 128;
      Sdl.Render.draw_line game.renderer
        ((0, 250), (x, y + 8));

      Sdl.Render.set_draw_color game.renderer (0, 0, 0) 255;
  end;


  (* Print message at bottom of the screen *)
  let s = Printf.sprintf "Lives: %d, Score: %d (Press TAB to change enemies)" game.life game.score in
  let w, h = Sdlttf.size_utf8 game.font s in
  let t = create_text_texture game s in
  let dst_rect = {
    Sdlrect.x = 0;
    Sdlrect.y = 500 - h;
    Sdlrect.w = w;
    Sdlrect.h = h;
  }
  in
  Sdl.Render.fill_rect game.renderer dst_rect;
  Sdl.Render.copy game.renderer ~texture: t ~dst_rect ();
  Sdl.Render.render_present game.renderer

let handle_events game event =
  let open Sdl.Event in
  match event with
  | Text_Input ti ->
    on_input game ti.ti_text
  | KeyDown ke ->
    if ke.scancode = Sdlscancode.TAB then
      game.current_enemy <- None
  | _ -> ()
