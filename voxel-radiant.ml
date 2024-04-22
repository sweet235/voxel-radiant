#!/usr/bin/env ocaml


(*
 * vectors and matrices
 *)

type 'a vec2 = 'a * 'a
type 'a vec3 = 'a * 'a * 'a
type 'a mat3 = 'a vec3 * 'a vec3 * 'a vec3

let ( ++ ) : int vec2 -> int vec2 -> int vec2
  = fun (x, y) (x0, y0) -> (x + x0, y + y0)
let ( +++ ) : int vec3 -> int vec3 -> int vec3
  = fun (x, y, z) (x0, y0, z0) -> (x + x0, y + y0, z + z0)
let ( *** ) : int -> int vec3 -> int vec3
  = fun s (x, y, z) -> (s * x, s * y, s * z)
let ( --- ) : int vec3 -> int vec3 -> int vec3
  = fun v v0 -> v +++ (-1) *** v0
let ( /// ) : int vec3 -> int -> int vec3
  = fun (x, y, z) s -> (x / s, y / s, z / s)
let ( ***~ ) : int vec3 -> int vec3 -> int vec3
  = fun (x, y, z) (x', y', z') -> (x * x', y * y', z * z')
let ( ***| ) : int mat3 -> int vec3 -> int vec3
  = fun ((a, b, c), (d, e, f), (g, h, i)) (x, y, z) ->
  (c*z + b*y + a*x, f*z + e*y + d*x, i*z + h*y + g*x)

let ident : int mat3 = ((1, 0, 0), (0, 1, 0), (0, 0, 1))
let rotz90 : int mat3 = ((0, -1, 0), (1, 0, 0), (0, 0, 1))
let rotzm90 : int mat3 = ((0, 1, 0), (-1, 0, 0), (0, 0, 1))
let rotz180 : int mat3 = ((-1, 0, 0), (0, -1, 0), (0, 0, 1))
let rotations = [ident; rotz90; rotz180; rotzm90]


(*
 * brush types
 *)

type scaling = float vec2
type shift = int vec2
type texture = Texture of (string * scaling * shift)
type surf = Surf of int vec3 * int vec3 * int vec3 * texture * bool
type brush = Cuboid of surf * surf * surf * surf * surf * surf


(*
 * global options
 *)

let cfg_ceiling_tex = ref @@ Texture ("shared_tech/floortile1a", (0.125, 0.125), (0, 0))
let cfg_cell_dim = ref (256, 256, 256)
let cfg_floor_tex = ref @@ Texture ("shared_tech/floortile1c", (0.125, 0.125), (0, 0))
let cfg_ladder_tex = ref @@ Texture ("shared_tech/floortile1b", (0.0625, 0.0625), (0, 0))
let cfg_lamp_tex = ref @@ Texture ("shared_trak5/light2_white_1500", (0.5, 0.5), (64, 64))
let cfg_lamp_width = ref 64
let cfg_sky_tex = ref @@ Texture ("shared_space/sky01", (0.25, 0.25), (0, 0))
let cfg_wall_tex = ref @@ Texture ("shared_tech/floortile1b", (0.125, 0.125), (0, 0))
let cfg_wall_thickness = ref 32
let cfg_ladders = ref true


(*
 * brush operations
 *)

let string_of_int_vec3 : int vec3 -> string
  = fun (x, y, z) -> Printf.sprintf "( %d %d %d )" x y z

let string_of_surf : surf -> string
  = function
  | Surf (v0, v1, v2, Texture (name, (scal0, scal1), (shift0, shift1)), structure) ->
     let flag = if structure then "0" else "134217728" in
     Printf.sprintf "%s %s %s %s %s %s 0 %s %s %s %s 0"
       (string_of_int_vec3 v0)
       (string_of_int_vec3 v1)
       (string_of_int_vec3 v2)
       name
       (string_of_int shift0)
       (string_of_int shift1)
       (string_of_float scal0)
       (string_of_float scal1)
       flag flag

let string_of_brush : brush -> string
  = function
  | Cuboid (s0, s1, s2, s3, s4, s5) ->
     Printf.sprintf "{\n%s\n%s\n%s\n%s\n%s\n%s\n}\n"
       (string_of_surf s0)
       (string_of_surf s1)
       (string_of_surf s2)
       (string_of_surf s3)
       (string_of_surf s4)
       (string_of_surf s5)

let translate_surf : surf -> int vec3 -> surf
  = fun (Surf (v0, v1, v2, tex, str)) v ->
  Surf (v0 +++ v, v1 +++ v, v2 +++ v, tex, str)

let rotate_surf  : surf -> int mat3 -> surf
  = fun (Surf (v0, v1, v2, tex, str)) mat ->
  Surf (mat ***| v0, mat ***| v1, mat ***| v2, tex, str)

let translate_brush : brush -> int vec3 -> brush
  = fun brush v ->
  let f s = translate_surf s v in
  match brush with
  | Cuboid (s0, s1, s2, s3, s4, s5) ->
     Cuboid (f s0, f s1, f s2, f s3, f s4, f s5)

let translate_brushes : brush list -> int vec3 -> brush list
  = fun brushes v ->
  List.map (fun x -> translate_brush x v) brushes

let rotate_brush : brush -> int mat3 -> brush
  = fun brush mat ->
  let f s = rotate_surf s mat in
  match brush with
  | Cuboid (s0, s1, s2, s3, s4, s5) ->
     Cuboid (f s0, f s1, f s2, f s3, f s4, f s5)

let rotate_brushes : brush list -> int mat3 -> brush list
  = fun brushes mat ->
  List.map (fun x -> rotate_brush x mat) brushes

(* create a cobuid, centered at the origin
   requires the dimensions to be even numbers, to center exactly *)
let create_cuboid : int vec3 -> texture -> texture -> texture -> texture -> texture ->
                    texture -> bool -> brush
  = fun (x, y, z) tleft tright tfront tback tbottom ttop structure ->
  assert (x mod 2 == 0);
  assert (y mod 2 == 0);
  assert (z mod 2 == 0);
  assert (z > 0);
  let bottom = Surf ((x, 0, z), (0, 0, z), (x, y, z), tbottom, structure) in
  let top = Surf ((0, y, 0), (0, 0, 0), (x, y, 0), ttop, structure) in
  let left = Surf ((0, y, 0), (0, y, z), (0, 0, 0), tleft, structure) in
  let right = Surf ((x, 0, z), (x, y, z), (x, 0, 0), tright, structure) in
  let front = Surf ((0, y, 0), (x, y, 0), (0, y, z), tfront, structure) in
  let back = Surf ((x, 0, z), (x, 0, 0), (0, 0, z), tback, structure) in
  translate_brush (Cuboid (bottom, top, left, right, front, back)) ((-1) *** (x, y, z) /// 2)


(*
 * representing the input file
 *)

type ascii_art = char array array array

let ascii_get : ascii_art -> int vec3 -> char option
  = fun arr (row, col, ply) ->
  if ply < 0 || ply >= Array.length arr then None
  else
    let the_ply = Array.get arr ply in
    if row < 0 || row >= Array.length the_ply then None
    else
      let the_row = Array.get the_ply row in
      if col < 0 || col >= Array.length the_row then None
      else
        match Array.get the_row col with
        | ' ' | '_' -> None
        | c -> Some c

let is_cell : ascii_art -> int vec3 -> bool
  = fun ascii_art v ->
  match ascii_get ascii_art v with
  | None | Some ' ' | Some '_' -> false
  | _ -> true

let array3_dim : ascii_art -> int vec3
  = fun arr ->
  let num_plies = Array.length arr in
  let num_lines = Array.length (Array.get arr 0) in
  let num_cols = Array.length (Array.get (Array.get arr 0) 0) in
  (num_lines, num_cols, num_plies)


(*
 * parsing the input file
 *)

let string_starts_with str prefix =
  let slen = String.length str in
  let plen = String.length prefix in
  if plen > slen then false
  else String.sub str 0 plen = prefix

let input_lines : string -> (string list, string) result
  = fun path ->
  let try_open_in p =
    try Some (open_in p) with Sys_error _ -> None in
  match try_open_in path with
  | None -> Error "could not open file"
  | Some infile ->
     let rec loop lines =
       try
         let line = input_line infile in
         loop (line :: lines)
       with End_of_file ->
         close_in infile;
         Ok (List.rev lines) in
     loop []

let split_lines_into_plies : string list -> string list list
  = fun lines ->
  let rec loop lines acc plies =
    match lines with
    | line :: lines when string_starts_with line "#ply" ->
       loop lines [] (List.rev acc :: plies)
    | line :: lines ->
       loop lines (line :: acc) plies
    | [] ->
       List.rev (List.rev acc :: plies) in
  loop lines [] []

let list_of_string : string -> char list
  = fun s ->
  let rec loop i l = if i < 0 then l else loop (i - 1) (s.[i] :: l) in
  loop (String.length s - 1) []

let parse_input_lists : string list list -> ascii_art
  = fun plies ->
  let lists = List.map (fun p -> List.map list_of_string p) plies in
  Array.of_list @@ List.map (fun p -> Array.of_list (List.map (fun x -> Array.of_list x) p)) lists

let ascii_art_limits : ascii_art -> int vec3 * int vec3
  = fun ascii ->
  let min_ply, min_row, min_col = ref max_int, ref max_int, ref max_int in
  let max_ply, max_row, max_col = ref min_int, ref min_int, ref min_int in
  for ply = 0 to Array.length ascii - 1 do
    for row = 0 to Array.length ascii.(ply) - 1 do
      for col = 0 to Array.length ascii.(ply).(row) - 1 do
        if is_cell ascii (row, col, ply) then
          begin
            if ply < !min_ply then min_ply := ply;
            if row < !min_row then min_row := row;
            if col < !min_col then min_col := col;
            if ply > !max_ply then max_ply := ply;
            if row > !max_row then max_row := row;
            if col > !max_col then max_col := col;
          end
      done
    done
  done;
  ((!min_row, !min_col, !min_ply), (!max_row, !max_col, !max_ply))

let crop_ascii_art : ascii_art -> ascii_art
  = fun ascii ->
  let ((min_row, min_col, min_ply) as mins),
      ((max_row, max_col, max_ply) as maxs) = ascii_art_limits ascii in
  let ((rows, cols, plies) as dims) = maxs --- mins +++ (1, 1, 1) in
  let cropped = Array.make plies (Array.make 0 (Array.make 0 ' ')) in
  for ply = 0 to plies - 1 do
    cropped.(ply) <- Array.make rows (Array.make cols ' ');
    for row = 0 to rows - 1 do
      cropped.(ply).(row) <- Array.make cols ' ';
      for col = 0 to cols - 1 do
        let pos = (row, col, ply) +++ mins in
        let char = match ascii_get ascii pos with None -> ' ' | Some c -> c in
        cropped.(ply).(row).(col) <- char;
      done
    done
  done;
  cropped

let parse_input : string list -> ascii_art
  = fun ls ->
  let ply_lists = split_lines_into_plies ls in
  crop_ascii_art @@ parse_input_lists ply_lists


(*
 * creating a cell
 *)

let caulk = Texture ("common/caulk", (1.0, 1.0), (0, 0))
let ladder = Texture ("common/ladder", (1.0, 1.0), (0, 0))
let playerclip = Texture ("common/playerclip", (1.0, 1.0), (0, 0))

let rec map_acc : ('a -> 'b list -> 'b list) -> 'b list -> 'a list -> 'b list
  = fun f acc ls -> match ls with
  | [] -> acc
  | x :: xs -> map_acc f (f x acc) xs

let is_sky_cell : ascii_art -> int vec3 -> bool
  = fun ascii_art (_, _, ply) -> ply = Array.length ascii_art - 1

let ceiling_with_lamp : unit -> brush list
  = fun () ->
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  let translate brush dx dy =
    translate_brush brush ((dx - !cfg_lamp_width) / 4, (dy + !cfg_lamp_width) / 4, 0) in
  let create dx dy =
    let brush =
      create_cuboid ((dx + !cfg_lamp_width) / 2, (dy - !cfg_lamp_width) / 2, !cfg_wall_thickness)
        caulk caulk caulk caulk caulk !cfg_ceiling_tex true in
    translate brush dx dy in
  let brush0 = create dim_x dim_y in
  let brush1 = create dim_y dim_x in
  let lamp_brush = create_cuboid (!cfg_lamp_width, !cfg_lamp_width, !cfg_wall_thickness)
                     caulk caulk caulk caulk caulk !cfg_lamp_tex true in
  let brushes = [lamp_brush; brush0; rotate_brush brush1 rotz90;
                 rotate_brush brush0 rotz180; rotate_brush brush1 rotzm90] in
  translate_brushes brushes (0, 0, dim_z / 2 + !cfg_wall_thickness / 2)

let wall_has_ladder : ascii_art -> int vec3 -> int vec3 -> bool
  = fun ascii_art ((row, col, ply) as pos) forward ->
  let (_, _, lim_z) = array3_dim ascii_art in
  let rec needs_ladder_up i =
    if ply + i >= lim_z then false
    else if not @@ is_cell ascii_art (pos +++ (0, 0, i)) then false
    else if is_cell ascii_art (pos +++ forward +++ (0, 0, i)) then true
    else needs_ladder_up (i + 1) in
  let rec needs_ladder_down i =
    if ply - i < 0 then true
    else if not @@ is_cell ascii_art (pos --- (0, 0, i)) then true
    else if is_cell ascii_art (pos --- (0, 0, i))
            && is_cell ascii_art (pos +++ forward --- (0, 0, i)) then false
    else needs_ladder_down (i + 1) in
  needs_ladder_up 1 && needs_ladder_down 1

let create_cell : ascii_art -> int vec3 -> brush list
  = fun ascii_art ((row, col, ply) as pos) ->
  let result = [] in
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in

  (* floor if needed *)
  let result = 
    if not @@ is_cell ascii_art (pos +++ (0, 0, -1)) then
      let brush = create_cuboid (dim_x, dim_y, !cfg_wall_thickness)
                    caulk caulk caulk caulk !cfg_floor_tex caulk true in
      let brush = translate_brush brush (0, 0, -dim_z / 2 - !cfg_wall_thickness / 2) in
      brush :: result
    else result in

  (* ceiling if needed *)
  let result =
    if not @@ is_cell ascii_art (pos +++ (0, 0, 1)) then
      let brushes =
        if is_sky_cell ascii_art pos then
          let brush = create_cuboid (dim_x, dim_y, !cfg_wall_thickness)
                        caulk caulk caulk caulk caulk !cfg_sky_tex true in
          let brush = translate_brush brush (0, 0, dim_z / 2 + !cfg_wall_thickness / 2) in
          [brush]
        else
          ceiling_with_lamp () in
      brushes @ result
    else result in

  (* walls if needed *)
  let one_side mat result =
    let forward = mat ***| (-1, 0, 0) in
    if not @@ is_cell ascii_art (pos +++ forward) then
      let needs_ladder = wall_has_ladder ascii_art pos forward in
      let create t0 t1 t2 t3 t4 t5 delta =
        let (dx, dy, _) = mat ***| !cfg_cell_dim in
        let brush = create_cuboid (!cfg_wall_thickness, abs dy, dim_z)
                      t0 t1 t2 t3 t4 t5 true in
        let brush = rotate_brush brush mat in
        let shift = mat ***| (-(abs dx + !cfg_wall_thickness - delta) / 2, 0, 0) in
        translate_brush brush shift in
      let tex = if !cfg_ladders && needs_ladder then !cfg_ladder_tex else !cfg_wall_tex in
      let result = create caulk tex caulk caulk caulk caulk 0 :: result in
      if !cfg_ladders then
        let inv = if needs_ladder then ladder else playerclip in
        create inv inv inv inv inv inv 1 :: result
      else result
    else result in
  let result = map_acc one_side result rotations in

  (* move the brushes to their place on the grid *)
  translate_brushes result (row * dim_x + dim_x / 2, col * dim_y + dim_y / 2, ply * dim_z + dim_z / 2)


(*
 * creating all cells
 *)

let compile_ascii_art : ascii_art -> brush list
  = fun ascii_art ->
  let brushes = ref [] in
  let (num_rows, num_cols, num_plies) = array3_dim ascii_art in
  for ply = 0 to num_plies - 1 do
    for row = 0 to num_rows - 1 do
      for col = 0 to num_cols - 1 do
        let pos = (row, col, ply) in
        if is_cell ascii_art pos then
          let cell = create_cell ascii_art pos in
          brushes := cell :: !brushes
      done
    done
  done;
  List.flatten !brushes

let write_map : brush list -> out_channel -> unit
  = fun brushes stream ->
  let put str = output_string stream str in
  put "{\n";
  put "\"classname\" \"worldspawn\"\n";
  let put_brush brush =
    put @@ string_of_brush brush in
  List.iter put_brush brushes;
  put "}\n"


(*
 * intermission
 *)

let intermission_string : int -> int -> int -> string
  = fun x y z ->
  Printf.sprintf "{
\"classname\" \"info_player_intermission\"
\"origin\" \"%d.000000 %d.000000 %d.000000\"
\"angle\" \"0.000000\"
}
" x y z

let write_intermission : ascii_art -> out_channel -> unit
  = fun arr stream ->
  let (num_lines, num_cols, num_plies) = array3_dim arr in
  for ply = 0 to num_plies - 1 do
    for line = 0 to num_lines - 1 do
      for col = 0 to num_cols - 1 do
        match ascii_get arr (line, col, ply) with
        | Some 'R' ->
           let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
           output_string stream (intermission_string (line * dim_x + 10) (col * dim_y + 128)
             (ply * dim_z + 152))
        | _ -> ()
      done
    done
  done


(*
 * buildings
 *)

type building = Building of int vec3 * string

let string_of_building : building -> string
  = fun (Building ((x, y, z), n)) ->
  Printf.sprintf "{
\"classname\" \"team_%s\"
\"origin\" \"%d.000000 %d.000000 %d.000000\"
\"angle\" \"0.000000\"
}
" n x y z

let translate_building : building -> int vec3 -> building
  = fun (Building (v, name)) v0 ->
  Building (v +++ v0, name)

let translate_buildings : building list -> int vec3 -> building list
  = fun bs v -> List.map (fun b -> translate_building b v) bs

let rotate_building : building -> int mat3 -> building
  = fun (Building (v, name)) mat ->
  Building (mat ***| v, name)

let rotate_buildings : building list -> int mat3 -> building list
  = fun bs mat -> List.map (fun b -> rotate_building b mat) bs

let eggs_origin =
  let e = 60 in
  [ Building ((0, -e, 0), "alien_spawn")
  ; Building ((0, 0, 0), "alien_spawn")
  ; Building ((0, e, 0), "alien_spawn") ]

let dispatch_on_char : ascii_art -> int -> int -> int -> building list
  = fun arr line col ply ->
  let d = 25 in
  match arr.(ply).(line).(col) with
  | 'v' -> translate_buildings eggs_origin (256 - d, 128, 0)
  | '^' -> translate_buildings eggs_origin (d, 128, 0)
  | '>' ->
     let es = rotate_buildings eggs_origin rotz90 in
     translate_buildings es (128, 256 - d, 0)
  | '<' ->
     let es = rotate_buildings eggs_origin rotz90 in
     translate_buildings es (128, d, 0)
  | 'O' ->
     [Building ((128, 128, 0), "alien_overmind")]
  | 'B' ->
     [Building ((128, 128, 0), "alien_booster")]
  | 'R' ->
     [Building ((64, 128, 0), "human_reactor");
      Building ((148, 128, 0), "human_drill");
      Building ((64, 40, 0), "human_spawn");
      Building ((64, 256 - 40, 0), "human_spawn");
      Building ((128 + 32, 32, 0), "human_mgturret");
      Building ((128 + 32, 192 + 32, 0), "human_mgturret");
      Building ((192 + 32, 32, 0), "human_mgturret");
      Building ((192 + 32, 64 + 32, 0), "human_mgturret");
      Building ((192 + 32, 128 + 32, 0), "human_mgturret");
      Building ((192 + 32, 192 + 32, 0), "human_mgturret")
     ]
  | 'A' ->
     [Building ((50, 128, 0), "human_armoury");
      Building ((50, 50, 0), "human_medistat");
      Building ((50, 256 - 50, 0), "human_medistat");
      Building ((128, 40, 0), "human_spawn");
      Building ((128, 256 - 40, 0), "human_spawn");
      Building ((128, 128, 0), "human_mgturret");
      Building ((192 + 32, 32, 0), "human_mgturret");
      Building ((192 + 32, 64 + 32, 0), "human_mgturret");
      Building ((192 + 32, 128 + 32, 0), "human_mgturret");
      Building ((192 + 32, 192 + 32, 0), "human_mgturret")
     ]
  | _ -> []

let create_buildings : ascii_art -> building list
  = fun arr ->
  let buildings = ref [] in
  let (num_lines, num_cols, num_plies) = array3_dim arr in
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  for ply = 0 to num_plies - 1 do
    for line = 0 to num_lines - 1 do
      for col = 0 to num_cols - 1 do
        if is_cell arr (line, col, ply) then begin
            let bs = dispatch_on_char arr line col ply in
            let bs = translate_buildings bs (line * dim_x, col * dim_y, 32 + ply * dim_z) in
            buildings := bs :: !buildings
          end
      done
    done
  done;
  List.flatten !buildings

let write_buildings : ascii_art -> out_channel -> unit
  = fun arr stream ->
  let bs = create_buildings arr in
  List.iter (fun b -> output_string stream (string_of_building b)) bs


(*
 * writing a navcon file
 *)

let write_navcons : ascii_art -> int -> int -> out_channel -> unit
  = fun arr down_max up_max stream ->
  let (num_rows, num_cols, num_plies) = array3_dim arr in
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  let dist_from_edge_bottom = 50 in
  let dist_from_edge_top = 50 in
  let exists pos = is_cell arr pos in
  output_string stream "navcon 3\n";
  for ply = 0 to num_plies - 1 do
    for row = 0 to num_rows - 1 do
      for col = 0 to num_cols - 1 do
        let pos = (row, col, ply) in
        let forward = (1, 0, 0) in
        let down = (0, 0, -1) in
        let top = (dim_x / 2 - dist_from_edge_top,
                   dim_y / 2 - dist_from_edge_top,
                   10) in
        let bottom = (dim_x / 2 + dist_from_edge_bottom,
                      dim_y / 2 + dist_from_edge_bottom,
                      0) in
        let base = (row * dim_x + dim_x / 2,
                    col * dim_y + dim_y / 2,
                    ply * dim_z) in
        let f mat =
          let forward = mat ***| forward in
          if exists pos
             && exists (pos +++ forward)
             && not (exists (pos +++ down)) then
            let up_allowed = ref true in
            let rec down_loop i =
              if exists (pos +++ i *** down) then
                up_allowed := false;
              if ply - i < 0 then -1
              else if not (exists (pos +++ forward +++ i *** down)) then -1
              else if exists (pos +++ forward +++ i *** down)
                      && not (exists (pos +++ forward +++ (i + 1) *** down)) then i
              else down_loop (i + 1) in
            let down_dist = down_loop 1 in
            if down_dist > 0 then
              let sel = forward +++ (0, 0, 1) in
              let upper_end = base +++ (top ***~ sel) in
              let lower_end = base +++ (bottom ***~ sel) +++ down_dist *** (0, 0, -dim_z) in
              let goes_up = down_dist * dim_z <= up_max && !up_allowed in
              let goes_down = down_dist * dim_z <= down_max in
              let twoway = if goes_up && goes_down then "1" else "0" in
              let (x, y, z), (x', y', z') =
                match goes_up, goes_down with
                | true, false -> lower_end, upper_end
                | _ -> upper_end, lower_end in
              let line = Printf.sprintf "%d %d %d %d %d %d 50 1 63 %s\n" x z y x' z' y' twoway in
              output_string stream line
        in List.iter f rotations
      done
    done
  done


(*
 * parsing options
 *)

let eat_option_lines : string list -> string list
  = fun lines ->
  let tokens line =
    List.filter (fun s -> String.length s > 0) (String.split_on_char ' ' line) in
  let die line =
    Printf.printf "syntax error: %s\n" line;
    exit 1 in
  let rec loop = function
    | line :: lines -> begin
        let parse_tex cfg = function
          | [texture; scal_x; scal_y; offs_x; offs_y] ->
             cfg := (try Texture (texture,
                                  (float_of_string scal_x, float_of_string scal_y),
                                  (int_of_string offs_x, int_of_string offs_y));
                     with _ -> die line)
          | _ -> die line in
        match tokens line with
        | "#sky_tex" :: rest -> parse_tex cfg_sky_tex rest; loop lines
        | "#floor_tex" :: rest -> parse_tex cfg_floor_tex rest; loop lines
        | "#wall_tex" :: rest -> parse_tex cfg_wall_tex rest; loop lines
        | "#ceiling_tex" :: rest -> parse_tex cfg_ceiling_tex rest; loop lines
        | "#lamp_tex" :: rest -> parse_tex cfg_lamp_tex rest; loop lines
        | ["#cell_dim"; dim_x; dim_y; dim_z] ->
           cfg_cell_dim :=
             (try let x = int_of_string dim_x in
                  let y = int_of_string dim_y in
                  let z = int_of_string dim_z in
                  assert (x mod 4 == 0);
                  assert (y mod 4 == 0);
                  assert (z mod 4 == 0);
                  (x, y, z)
              with _ -> die line);
           loop lines
        | ["#lamp_width"; width] ->
           cfg_lamp_width := (try int_of_string width with _ -> die line);
           loop lines
        | ["#ladders"; "off"] ->
           cfg_ladders := false;
           loop lines
        | ["#ply"] ->
           loop lines
        | t :: ts when t.[0] == '#' ->
           die line
        | _ ->
           line :: lines
      end
    | [] -> assert false in
  loop lines


(*
 * putting everything together
 *)

let map_name_of_dpkdir : string -> string
  = fun src_dpkdir ->
  let start_pos =
    match String.rindex_opt src_dpkdir '/' with
    | None -> 0
    | Some n -> n in
  let name_with_prefix =
    match String.index_from_opt src_dpkdir start_pos '_' with
    | None -> failwith "invalid dpkdir name"
    | Some n ->
       String.sub src_dpkdir start_pos (n - start_pos) in
  match String.sub name_with_prefix 0 4 with
  | "map-" -> String.sub name_with_prefix 4 (String.length name_with_prefix - 4)
  | _ -> failwith "map name must start with map-"

let main : string -> string -> unit
  = fun input_path output_path ->
  let map_name = map_name_of_dpkdir output_path in
  let map_source_path = output_path ^ "/maps/" ^ map_name ^ ".map" in
  match input_lines input_path with
  | Error msg -> print_string (msg ^ "\n")
  | Ok lines ->
     let lines = eat_option_lines lines in
     let arr = parse_input lines in
     let (_, _, dim_z) = array3_dim arr in if dim_z = 1 then cfg_ladders := false;
     let brushes = compile_ascii_art arr in
     let stream = open_out map_source_path in
     write_map brushes stream;
     write_intermission arr stream;
     write_buildings arr stream;
     close_out stream;
     let f (classname, down_max, up_max) =
       let stream = open_out (output_path ^ "/maps/" ^ map_name ^ "-" ^ classname ^ ".navcon") in
       write_navcons arr down_max up_max stream;
       close_out stream in
     List.iter f [
         ("builder", max_int, 0);
         ("builderupg", max_int, max_int);
         ("level0", max_int, max_int);
         ("level1", max_int, max_int);
         ("level2", max_int, 256);
         ("level2upg", max_int, 256);
         ("level3", max_int, 256);
         ("level3upg", max_int, 256);
         ("level4", max_int, 0);
         ("human_naked", 256, max_int);
         ("human_bsuit", 512, max_int);
       ]

let main_cmdline () =
    match Sys.argv with
  | [| own_name; input_path; output_path |] ->
     main input_path output_path
  | arr ->
     Printf.printf "Usage:\n    %s <ascii-art-file> <map-file>\n" arr.(0)

let () = main_cmdline ()
