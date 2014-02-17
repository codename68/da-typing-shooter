let _ =
  Settings.init ();
  Random.self_init ();
  Sdl.init [ `EVERYTHING ];
  Sdlttf.init ();
  Sdlimage.(init [ Init_PNG ]);
  let window =
    Sdl.Window.(create
      ~title: "test"
      ~pos: (`undefined, `undefined)
      ~dims: (500, 500)
      ~flags: [ Input_Focus; Mouse_Focus ])
  in
  let renderer = Sdl.Render.(create_renderer window (-1) [Accelerated;PresentVSync]) in
  let game = Game.init renderer in
  
  let score = ref 0 in
  let time = ref (Unix.gettimeofday ()) in
  let finished = ref false in
  while not !finished do
    begin
      let open Sdl.Event in
      match poll_event () with
      | None -> ()
      | Some event ->
        begin
          match event with
          | Quit _ ->
            finished := true
          | _ -> Game.handle_events game event
        end
    end;
    let time' = Unix.gettimeofday () in
    let delta = time' -. !time in
    time := time';
    begin
      try
        Game.think game delta
      with Game.Game_finished s ->
        finished := true;
        score := s
    end;
    Game.draw game     
  done;
  Game.quit game;
  Printf.printf "Final score: %d\n%!" !score;
  Sdlttf.quit ();
  Sdlimage.quit ();
  Sdl.quit ()
