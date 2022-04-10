(* A basic drawing app for demonstrating the use of Bogue.

   Copyright San Vu Ngoc, 2022

*)

open Tsdl
open Bogue
module W = Widget
module L = Layout
module E = Sdl.Event

type point = {
  x: int;
  y: int }

type stroke = {
  points : point list;
  color : Draw.color }

type canvas = {
  mutable current_color : Draw.color;
  mutable current_points : point list;
  strokes : stroke Queue.t }

let print s = Printf.ksprintf print_endline s

(* Shortcup to get the result of SDL functions *)
let go = Utils.go
let do_option o f = Option.iter f o
let plural s x = Printf.sprintf "%i %s%s" x s (if x > 1 then "s" else "")

let new_canvas () =
  { current_color = Draw.(opaque black);
    strokes = Queue.create ();
    current_points = [] }

let clear_canvas canvas =
  Queue.clear canvas.strokes

let record_point canvas p =
  canvas.current_points <- p::canvas.current_points;
  print "Recording x=%i y=%i" p.x p.y

(* Check if some button was pressed. *)
let mm_pressed ev =
  Int32.logand E.(get ev mouse_motion_state) (Sdl.Button.lmask) <> 0l

(* Get the (x,y) position associated with the mouse event. *)
let get_pos ev =
  match Trigger.event_kind ev with
  | `Mouse_motion
  | `Mouse_button_down  ->
    E.(get ev mouse_motion_x), E.(get ev mouse_motion_y)
  | _ -> failwith "Wrong event"

let record_point_event canvas area ev =
  if mm_pressed ev
  then let x,y = Sdl_area.pointer_pos area ev in
    let p = { x; y } in
    record_point canvas p;
    Sdl_area.update area

let click canvas _a a ev =
  let area = W.get_sdl_area a in
  record_point_event canvas area ev

let draw_stroke renderer stroke =
  Draw.set_color renderer stroke.color;
  match stroke.points with
  | [] -> ()
  | [p] ->
    Sdl.render_draw_point renderer p.x p.x |> go
  | list ->
    let points = List.rev_map (fun p -> Sdl.Point.create ~x:p.x ~y:p.y) list in
    Sdl.render_draw_lines renderer points |> go

let draw_current canvas renderer =
  let current_stroke = { color = canvas.current_color;
                         points = canvas.current_points } in
   draw_stroke renderer current_stroke

let draw_canvas canvas renderer =
  Queue.iter (draw_stroke renderer) canvas.strokes;
  draw_current canvas renderer

(* What to do when the mouse button is released: we ask the Sdl_area to draw the
   current stroke and cache the resulting texture for better performance. Then
   we move the current stroke into the queue and update the Sdl_area. *)
let release canvas a =
  let stroke = { color = canvas.current_color;
                 points = canvas.current_points } in
  Queue.add stroke canvas.strokes;
  print "Mouse released. We have now %s."
    (plural "stroke" (Queue.length canvas.strokes));
  let area = W.get_sdl_area a in
  (* The current points should be cleared just before saving the cache and just
     after drawing them... *)
  Sdl_area.add area (fun _ -> canvas.current_points <- []);
  Sdl_area.cache area;
  (* Caching has the effect to remove previously entered commands in the queue,
     so we have to add this back: *)
  Sdl_area.add area (draw_current canvas)

let pencil_icon canvas color =
  let fg = Draw.(opaque color) in
  let label = Label.icon ~fg ~size:24 "pencil" in
  let bg_off = Style.theme_bg  in
  let bg_on = Style.opaque_bg Draw.pale_grey in
  let w = W.button ~kind:Button.Switch ~label ~bg_on ~bg_off
      ~border_radius:12 "" in
  let c = W.connect_main w w (fun w _ _ ->
      if W.get_state w
      then (print "set color"; canvas.current_color <- fg))
      [E.mouse_button_up] in
  (* We should not add the connections here because they will be removed by
     Radiolist. *)
  w, c

let undo_icon canvas area =
  let area = W.get_sdl_area area in
  let label = Label.icon ~size:24 "undo" in
  let bg_off = Style.theme_bg  in
  let w = W.button ~kind:Button.Trigger ~label ~bg_off ~border_radius:12 "" in
  W.on_click w ~click:(fun _ ->
      if not (Queue.is_empty canvas.strokes)
      then let list = Queue.fold (fun l x -> x::l) [] canvas.strokes
                      |> List.tl in
        Queue.clear canvas.strokes;
        List.iter (fun x -> Queue.add x canvas.strokes) (List.rev list);
        Sdl_area.clear area;
        Sdl_area.add area (draw_canvas canvas);
        Sdl_area.cache area;
        Sdl_area.add area (draw_current canvas));
  w

let clear_btn canvas area =
  let w = W.button ~kind:Button.Trigger ~border_radius:6 "Clear" in
  W.on_release w ~release:(fun w ->
      if not (Queue.is_empty canvas.strokes)
      then do_option (Layout.containing_widget w) (fun layout ->
          Popup.yesno ~w:100 ~h:35 "Really clear?" (Layout.top_house layout)
            ~no_action:(fun () -> ())
            ~yes_action:(fun () ->
                clear_canvas canvas;
                let area = W.get_sdl_area area in
                Sdl_area.clear_cache area;
                Sdl_area.update area)));
  w

let toolbar canvas area =
  let colors = Draw.[black; red; find_color "forestgreen";
                     find_color "royalblue"; find_color "orchid"; white] in
  let pencils, cx = List.split @@ List.map (pencil_icon canvas) colors in
  let _ = Radiolist.of_widgets ~selected:0 pencils in
  List.iter2 W.add_connection pencils cx;
  let bar = L.flat ~background:L.theme_bg ~name:"toolbar" ~align:Draw.Center
      [ L.flat_of_w  pencils;
        Space.hfill ();
        L.flat_of_w ~align:Draw.Center
          [undo_icon canvas area; clear_btn canvas area] ] in
  L.fix_content bar;
  Space.full_width bar;
  bar

let area canvas =
  let style = Style.(of_bg theme_bg) in
  let a = W.sdl_area ~w:600 ~h:500 ~style () in
  let area = W.get_sdl_area a in
  Sdl_area.add area (fun renderer -> Draw.set_color renderer Draw.(opaque black));
  Sdl_area.add area (draw_current canvas);
  let c_rec = W.connect_main a a (fun _ _ -> record_point_event canvas area)
      [E.mouse_motion] in
  let c_draw = W.connect_main a a (fun _ _ _ -> Sdl_area.update area)
      [E.mouse_motion] in
  W.on_release ~release:(release canvas) a;
  let c_click = W.connect_main a a (click canvas) [E.mouse_button_down] in
  [c_rec; c_draw; c_click], a

let main () =
  let canvas = new_canvas () in
  let c_area, area = area canvas in
  let toolbar = toolbar canvas area in
  let layout = L.resident area in
  let window = L.tower ~name:"Bogue Drawing" [toolbar; layout] in
  Bogue.(run (make c_area [window]))

let () = main ()
