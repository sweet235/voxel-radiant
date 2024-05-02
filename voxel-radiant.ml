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
type texture = Texture of (string * scaling * shift * float)
type surf = Surf of int vec3 * int vec3 * int vec3 * texture * bool
type brush = Cuboid of surf * surf * surf * surf * surf * surf


(*
 * global options
 *)

let cfg_ceiling_tex = ref @@ Texture ("shared_tech/floortile1a", (0.125, 0.125), (0, 0), 0.0)
let cfg_cell_dim = ref (256, 256, 256)
let cfg_floor_tex = ref @@ Texture ("shared_tech/floortile1c", (0.125, 0.125), (0, 0), 0.0)
let cfg_ladder_tex = ref @@ Texture ("shared_tech/floortile1b", (0.0625, 0.0625), (0, 0), 0.0)
let cfg_lamp_tex = ref @@ Texture ("shared_trak5/light2_white_1500", (0.5, 0.5), (64, 64), 0.0)
let cfg_lamp_width = ref 64
let cfg_sky_tex = ref @@ Texture ("shared_space/sky01", (0.25, 0.25), (0, 0), 0.0)
let cfg_wall_tex = ref @@ Texture ("shared_tech/floortile1b", (0.125, 0.125), (0, 0), 0.0)
let cfg_wall_tex_ply : (int, texture) Hashtbl.t = Hashtbl.create 64
let cfg_wall_tex_random : texture array option ref = ref None
let cfg_wall_tex_ladder = ref @@ !cfg_wall_tex
let cfg_wall_thickness = ref 32
let cfg_ladders = ref true
let cfg_ladder_width = ref 96
let cfg_single_sky = ref false
let cfg_eggs_num = ref 3
let cfg_eggs_dist = ref 60
let cfg_double_floor_depth = ref 32
let cfg_double_floor_width = ref 96
let cfg_double_floor_tex = ref @@ Texture ("shared_tech/floortile1b", (0.125, 0.125), (0, 0), 0.0)

let get_cfg_wall_tex : int -> texture
  = fun ply ->
  match !cfg_wall_tex_random with
  | None ->
     begin try Hashtbl.find cfg_wall_tex_ply ply with _ -> !cfg_wall_tex end
  | Some array ->
     let len = Array.length array in
     array.(Random.int len)

(*
 * brush operations
 *)

let string_of_int_vec3 : int vec3 -> string
  = fun (x, y, z) -> Printf.sprintf "( %d %d %d )" x y z

let string_of_surf : surf -> string
  = function
  | Surf (v0, v1, v2, Texture (name, (scal0, scal1), (shift0, shift1), rot), structure) ->
     let flag = if structure then "0" else "134217728" in
     Printf.sprintf "%s %s %s %s %s %s %s %s %s %s %s 0"
       (string_of_int_vec3 v0)
       (string_of_int_vec3 v1)
       (string_of_int_vec3 v2)
       name
       (string_of_int shift0)
       (string_of_int shift1)
       (string_of_float rot)
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

let translate_surf : int vec3 -> surf -> surf
  = fun v (Surf (v0, v1, v2, tex, str)) ->
  Surf (v0 +++ v, v1 +++ v, v2 +++ v, tex, str)

let rotate_surf  : int mat3 -> surf -> surf
  = fun mat (Surf (v0, v1, v2, tex, str)) ->
  Surf (mat ***| v0, mat ***| v1, mat ***| v2, tex, str)

let translate_brush : int vec3 -> brush -> brush
  = fun v brush ->
  let f = translate_surf v in
  match brush with
  | Cuboid (s0, s1, s2, s3, s4, s5) ->
     Cuboid (f s0, f s1, f s2, f s3, f s4, f s5)

let translate_brushes : int vec3 -> brush list -> brush list
  = fun v brushes ->
  List.map (translate_brush v) brushes

let rotate_brush : int mat3 -> brush -> brush
  = fun mat brush ->
  let f = rotate_surf mat in
  match brush with
  | Cuboid (s0, s1, s2, s3, s4, s5) ->
     Cuboid (f s0, f s1, f s2, f s3, f s4, f s5)

let rotate_brushes : int mat3 -> brush list -> brush list
  = fun mat brushes ->
  List.map (rotate_brush mat) brushes

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
  translate_brush ((-1) *** (x, y, z) /// 2) (Cuboid (bottom, top, left, right, front, back))


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

let caulk = Texture ("common/caulk", (1.0, 1.0), (0, 0), 0.0)
let ladder = Texture ("common/ladder", (1.0, 1.0), (0, 0), 0.0)
let playerclip = Texture ("common/playerclip", (1.0, 1.0), (0, 0), 0.0)
let glass = Texture ("shared_trak5/glass", (1.0, 1.0), (0, 0), 0.0)

let rec map_acc : ('a -> 'b list -> 'b list) -> 'a list -> 'b list -> 'b list
  = fun f ls acc -> match ls with
  | [] -> acc
  | x :: xs -> map_acc f xs (f x acc)

let is_sky_cell : ascii_art -> int vec3 -> bool
  = fun ascii_art (_, _, ply) -> ply = Array.length ascii_art - 1

let ceiling_with_lamp : ascii_art -> int vec3 -> brush list
  = fun ascii_art pos ->
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  let translate brush dx dy =
    translate_brush ((dx - !cfg_lamp_width) / 4, (dy + !cfg_lamp_width) / 4, 0) brush in
  let create mat forward right =
    let (dx, dy, _) = mat ***| !cfg_cell_dim in
    let dx, dy = abs dx, abs dy in
    let forward = mat ***| forward in
    let right = mat ***| right in
    let len, len_shift = if not @@ is_cell ascii_art (pos +++ forward) then
                       (dy - !cfg_lamp_width) / 2, 0
                     else
                       (dy - !cfg_lamp_width) / 2 - !cfg_wall_thickness, -2 * !cfg_wall_thickness in
    let width, width_shift = if not @@ is_cell ascii_art (pos +++ right) then
                               (dx + !cfg_lamp_width) / 2, 0
                             else
                               (dx + !cfg_lamp_width) / 2 - !cfg_wall_thickness, -2 * !cfg_wall_thickness in
    let brush = create_cuboid (width, len, !cfg_wall_thickness)
                  caulk caulk caulk caulk caulk !cfg_ceiling_tex true in
    let brush = translate brush (dx + width_shift) (dy + len_shift) in
    rotate_brush mat brush in
  let lamp_brush = create_cuboid (!cfg_lamp_width, !cfg_lamp_width, !cfg_wall_thickness)
                     caulk caulk caulk caulk caulk !cfg_lamp_tex true in
  lamp_brush :: List.map (fun m -> create m (0, 1, 1) (1, 0, 1)) rotations
  |> translate_brushes (0, 0, dim_z / 2 + !cfg_wall_thickness / 2)

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

let create_wall_with_ladder
  = fun width has_floor wall ->
  let (_, _, dim_z) = !cfg_cell_dim in
  let strip_width = (width - !cfg_ladder_width) / 2 in
  let offs = (strip_width + !cfg_ladder_width) / 2 in
  let right = create_cuboid (!cfg_wall_thickness, strip_width, dim_z)
                caulk wall caulk wall !cfg_floor_tex !cfg_ceiling_tex true
              |> translate_brush (0, offs, 0) in
  let left = create_cuboid (!cfg_wall_thickness, strip_width, dim_z)
               caulk wall wall caulk !cfg_floor_tex !cfg_ceiling_tex true
             |> translate_brush (0, -offs, 0) in
  let center = create_cuboid (!cfg_wall_thickness, !cfg_ladder_width, dim_z)
                 caulk wall caulk caulk !cfg_floor_tex !cfg_ceiling_tex true
             |> translate_brush (-(!cfg_wall_thickness / 2), 0, 0) in
  let ladder = create_cuboid (!cfg_wall_thickness / 2, !cfg_ladder_width, dim_z)
                 ladder ladder ladder ladder ladder ladder true
             |> translate_brush (!cfg_wall_thickness / 4, 0, 0) in
  let result = [ladder; left; center; right] in
  let result =
    if not has_floor then result
    else
      let floor = create_cuboid (!cfg_wall_thickness, !cfg_ladder_width, !cfg_wall_thickness)
                    caulk caulk caulk caulk !cfg_floor_tex caulk true
                  |> translate_brush (0, 0, -(dim_z + !cfg_wall_thickness) / 2) in
      floor :: result in
  let tex = Texture ("shared_pk02/generic01b", (1.0, 1.0), (0, 0), 0.0) in
  let rec loop h acc =
    if h > dim_z then acc
    else
      let br = create_cuboid (4, !cfg_ladder_width, 4)
                 tex tex tex tex tex tex false
               |> translate_brush ((!cfg_wall_thickness / 4), 0, h - dim_z / 2) in
      loop (h + 32) (br :: acc) in
  loop 16 result

let create_double_floor
  = fun () ->
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  assert (dim_x = dim_y);
  let glass_brush = create_cuboid (!cfg_double_floor_width, !cfg_double_floor_width, 8)
                      caulk caulk caulk caulk glass caulk true
                    |> translate_brush (0, 0, -dim_z / 2 - 4) in
  let strip_width = (dim_x - !cfg_double_floor_width) / 2 in
  let side_one = create_cuboid (strip_width, dim_x - strip_width, !cfg_double_floor_depth)
                   !cfg_double_floor_tex caulk caulk caulk !cfg_double_floor_tex caulk true
                 |> translate_brush (dim_x / 2 - strip_width / 2,
                                     strip_width / 2,
                                     -(dim_z + !cfg_double_floor_depth) / 2) in
  glass_brush :: List.map (fun m -> rotate_brush m side_one) rotations

let create_glass_walls
  = fun () ->
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  assert (dim_x = dim_y);
  let thickness = 4 in
  let side_one = create_cuboid (dim_x, thickness, dim_z)
                   caulk caulk glass caulk caulk caulk false
                 |> translate_brush (0, dim_y / 2 - thickness / 2, 0) in
  List.map (fun m -> rotate_brush m side_one) rotations

let create_cell : ascii_art -> int vec3 -> brush list
  = fun ascii_art ((row, col, ply) as pos) ->
  let result = [] in
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  let exists pos = is_cell ascii_art pos in
  let wt = !cfg_wall_thickness in

  let result = match ascii_get ascii_art pos with
    | Some 'a' -> create_glass_walls () @ result
    | _ -> result in

  (* floor if needed *)
  let result = 
    if not @@ is_cell ascii_art (pos +++ (0, 0, -1)) then
      let f dim v v' =
        match exists (pos +++ v), exists (pos +++ v') with
        | false, false -> dim, 0
        | false, true -> dim - wt, wt / 2
        | true, false -> dim - wt, -wt / 2
        | true, true -> dim - 2 * wt, 0 in
      let width, width_shift = f dim_x (1, 0, -1) (-1, 0, -1) in
      let len, len_shift = f dim_y (0, 1, -1) (0, -1, -1) in
      let brush = create_cuboid (width, len, !cfg_wall_thickness)
                    caulk caulk caulk caulk !cfg_floor_tex caulk true
                  |> translate_brush (width_shift, len_shift, -dim_z / 2 - !cfg_wall_thickness / 2) in
      match ascii_get ascii_art pos with
      | Some 'm' ->
         let brush = translate_brush (0, 0, -(!cfg_double_floor_depth)) brush in
         (brush :: create_double_floor ()) @ result
      | _ -> brush :: result
    else result in

  (* ceiling if needed *)
  let result =
    if not @@ is_cell ascii_art (pos +++ (0, 0, 1)) then
      let brushes =
        if is_sky_cell ascii_art pos then
          if not !cfg_single_sky then
            let brush = create_cuboid (dim_x, dim_y, !cfg_wall_thickness)
                          caulk caulk caulk caulk caulk !cfg_sky_tex true
                        |> translate_brush (0, 0, dim_z / 2 + !cfg_wall_thickness / 2) in
            [brush]
          else []
        else
          ceiling_with_lamp ascii_art pos in
      brushes @ result
    else result in

  (* walls if needed *)
  let one_side mat result =
    let forward = mat ***| (-1, 0, 0) in
    if not @@ is_cell ascii_art (pos +++ forward) then
      let needs_ladder = wall_has_ladder ascii_art pos forward in
      let (dx, dy, _) = mat ***| !cfg_cell_dim in
      let create t0 t1 t2 t3 t4 t5 delta =
        let brushes = if not needs_ladder then
                        [create_cuboid (!cfg_wall_thickness, abs dy, dim_z)
                           t0 t1 t2 t3 t4 t5 true]
                      else
                        let needs_floor = not @@ exists (pos +++ (0, 0, -1))
                                          && not @@ exists (pos +++ mat ***| (-1, 0, -1)) in
                        create_wall_with_ladder (abs dy) needs_floor !cfg_wall_tex_ladder in
        let brushes = rotate_brushes mat brushes in
        let shift = mat ***| (-(abs dx + !cfg_wall_thickness - delta) / 2, 0, 0) in
        translate_brushes shift brushes in
      let tex = get_cfg_wall_tex ply in
      let result = create caulk tex caulk caulk !cfg_floor_tex !cfg_ceiling_tex 0 @ result in
      result
    else result in
  map_acc one_side rotations result
  (* move the brushes to their place on the grid *)
  |> translate_brushes (row * dim_x + dim_x / 2, col * dim_y + dim_y / 2, ply * dim_z + dim_z / 2)

let create_single_sky : ascii_art -> brush
  = fun ascii_art ->
  let (rows, cols, plies) = array3_dim ascii_art in
  let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
  let (x, y, z) = rows * dim_x, cols * dim_y, !cfg_wall_thickness in
  create_cuboid (x, y, z) caulk caulk caulk caulk caulk !cfg_sky_tex true
  |> translate_brush (x / 2, y / 2, plies * dim_z + !cfg_wall_thickness / 2)


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
  let f opt =
    let is_hit = match opt with Some needle -> fun c -> c = needle | None -> fun _ -> true in
    for ply = 0 to num_plies - 1 do
      for line = 0 to num_lines - 1 do
        for col = 0 to num_cols - 1 do
          match ascii_get arr (line, col, ply) with
          | Some c when is_hit c ->
             let (dim_x, dim_y, dim_z) = !cfg_cell_dim in
             output_string stream (intermission_string (line * dim_x + 10) (col * dim_y + 128)
                                     (ply * dim_z + 152));
             raise Exit
          | _ -> ()
        done
      done
    done in
  try List.iter f [Some 'R'; None] with Exit -> ()

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

let create_eggs
  = fun num dist ->
  let rec loop acc = function
    | -1 -> acc
    | n -> loop (Building ((0, n * dist, 0), "alien_spawn") :: acc) (n - 1) in
  let span = (num - 1) * dist in
  translate_buildings (loop [] (num - 1)) (0, -span / 2, 0)

let dispatch_on_char : ascii_art -> int -> int -> int -> building list
  = fun arr line col ply ->
  let eggs_origin = create_eggs !cfg_eggs_num !cfg_eggs_dist in
  let d = 25 in
  let (dim_x, dim_y, _) = !cfg_cell_dim in
  match arr.(ply).(line).(col) with
  | 'v' -> translate_buildings eggs_origin (dim_x - d, dim_y / 2, 0)
  | '^' -> translate_buildings eggs_origin (d, dim_y / 2, 0)
  | '>' ->
     let es = rotate_buildings eggs_origin rotz90 in
     translate_buildings es (dim_x / 2, dim_y - d, 0)
  | '<' ->
     let es = rotate_buildings eggs_origin rotz90 in
     translate_buildings es (dim_x / 2, d, 0)
  | 'O' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "alien_overmind")]
  | 'B' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "alien_booster")]
  | 'R' when dim_x >= 256 && dim_y >= 256 ->
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
  | 'R' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "human_reactor")]
  | 'A' when dim_x >= 256 && dim_y >= 256 ->
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
  | 'A' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "human_armoury")]
  | 'M' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "human_medistat")]
  | 'N' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "human_spawn")]
  | 'D' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "human_drill")]
  | 'T' ->
     [Building ((dim_x / 2, dim_y / 2 - 32, 0), "human_mgturret");
      Building ((dim_x / 2, dim_y / 2 + 32, 0), "human_mgturret")
     ]
  | 'a' ->
     [Building ((dim_x / 2, dim_y / 2, 0), "human_armoury")]
  | 'm' ->
     [Building ((dim_x / 2, dim_y / 2, -16 - 32), "human_medistat");
      Building ((dim_x / 2, dim_y / 2, -24 - 32), "human_medistat")
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

let write_navcons : ascii_art -> int -> int -> bool -> bool -> out_channel -> unit
  = fun arr down_max up_max pounces_up uses_arm stream ->
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
              if exists (pos +++ i *** down) && not pounces_up then
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
              output_string stream line in
        List.iter f rotations;
        if pounces_up && dim_x < 256 && dim_y < 256 && dim_z < 256 then
          begin
            let f mat =
              let forward = mat ***| forward in
              if exists pos
                 && not (exists (pos +++ down))
                 && exists (pos +++ forward)
                 && exists (pos +++ forward +++ down)
                 && exists (pos +++ forward +++ 2 *** down)
                 && exists (pos +++ 2 *** forward)
                 && exists (pos +++ 2 *** forward +++ down)
                 && not (exists (pos +++ 2 *** forward +++ 2 *** down)) then
                let sel = forward +++ (0, 0, 1) in
                let (x, y, z) = base +++ (top ***~ sel) in
                let (x', y', z') = base +++ (bottom ***~ sel) +++ (forward ***~ !cfg_cell_dim) +++ (0, 0, -dim_z) in
                let line = Printf.sprintf "%d %d %d %d %d %d 50 1 63 0\n" x' z' y' x z y in
                output_string stream line in
            List.iter f rotations;
          end;
        if uses_arm && ascii_get arr pos = Some 'a' then
          let f mat =
            let forward = mat ***| forward in
            if exists (pos +++ forward) && not (exists (pos +++ forward +++ down)) then
              let (x, y, z) = base +++ (forward ***~ (dim_x / 2 - 20, dim_y / 2 - 20, 0)) in
              let (x', y', z') = (pos +++ forward) ***~ (dim_x, dim_y, dim_z) +++ (dim_x / 2, dim_y / 2, 0) in
              let line = Printf.sprintf "%d %d %d %d %d %d 15 1 63 0\n" x' z' y' x z y in
              output_string stream line in
          List.iter f rotations;
      done
    done
  done


(*
 * parsing options
 *)

let ( let* ) = Result.bind

let eat_option_lines : string list -> (string list, string) result
  = fun lines ->
  let tokens line =
    List.filter (fun s -> String.length s > 0) (String.split_on_char ' ' line) in
  let error line =
    Error (Printf.sprintf "syntax error in: %s" line) in
  let rec loop = function
    | line :: lines -> begin
        let catch f = try f (); Ok () with _ -> error line in
        let parse_tex_setter setter rest =
          let f texture scal_x scal_y offs_x offs_y rot =
            catch @@ fun () ->
               setter @@ Texture (texture,
                                  (float_of_string scal_x, float_of_string scal_y),
                                  (int_of_string offs_x, int_of_string offs_y),
                                  float_of_string rot) in
          match rest with
          | [texture; scal_x; scal_y] ->
             f texture scal_x scal_y "0" "0" "0"
          | [texture; scal_x; scal_y; offs_x; offs_y] ->
             f texture scal_x scal_y offs_x offs_y "0"
          | [texture; scal_x; scal_y; offs_x; offs_y; rot] ->
             f texture scal_x scal_y offs_x offs_y rot
          | _ -> error line in
        let parse_tex cfg = parse_tex_setter (fun tex -> cfg := tex) in
        let parse_int cfg str =
          try cfg := int_of_string str; Ok () with _ -> error line in
        match tokens line with
        | "#sky_tex" :: rest -> let* () = parse_tex cfg_sky_tex rest in loop lines
        | "#floor_tex" :: rest -> let* () = parse_tex cfg_floor_tex rest in loop lines
        | "#double_floor_tex" :: rest -> let* () = parse_tex cfg_double_floor_tex rest in loop lines
        | "#wall_tex" :: rest -> let* () = parse_tex cfg_wall_tex rest in loop lines
        | "#wall_tex_ladder" :: rest -> let* () = parse_tex cfg_wall_tex_ladder rest in loop lines
        | "#ceiling_tex" :: rest -> let* () = parse_tex cfg_ceiling_tex rest in loop lines
        | "#lamp_tex" :: rest -> let* () = parse_tex cfg_lamp_tex rest in loop lines
        | "#wall_tex_ply" :: ply :: rest ->
           let* p = try Ok (int_of_string ply) with _ -> error line in
           let* () = parse_tex_setter (fun tex -> Hashtbl.add cfg_wall_tex_ply p tex) rest in
           loop lines
        | "#wall_tex_random" :: rest ->
           let* () = parse_tex_setter
                     (fun tex ->
                       let ls = match !cfg_wall_tex_random with
                         | None -> []
                         | Some a -> Array.to_list a in
                       let new_a = Array.of_list (tex :: ls) in
                       cfg_wall_tex_random := Some new_a) rest in
           loop lines
        | ["#cell_dim"; dim_x; dim_y; dim_z] ->
           let* () = catch @@ fun () ->
             cfg_cell_dim :=
               let x = int_of_string dim_x in
               let y = int_of_string dim_y in
               let z = int_of_string dim_z in
               assert (x mod 4 == 0);
               assert (y mod 4 == 0);
               assert (z mod 4 == 0);
               (x, y, z) in
           loop lines
        | ["#lamp_width"; width] ->
           let* () = catch @@ fun () -> cfg_lamp_width := int_of_string width in
           loop lines
        | ["#ladder_width"; width] ->
           let* () = catch @@ fun () -> cfg_ladder_width := int_of_string width in
           loop lines
        | ["#eggs_num"; n] ->
           let* () = parse_int cfg_eggs_num n in loop lines
        | ["#eggs_dist"; n] ->
           let* () = parse_int cfg_eggs_dist n in loop lines
        | ["#ladders"; "off"] ->
           cfg_ladders := false;
           loop lines
        | ["#single_sky"] ->
           cfg_single_sky := true;
           loop lines
        | ["#ply"] ->
           loop lines
        | t :: ts when t.[0] == '#' ->
           error line
        | _ ->
           Ok (line :: lines)
      end
    | [] -> Error "no rooms (append some + characters to the file)" in
  loop lines


(*
 * putting everything together
 *)

let map_name_of_dpkdir : string -> (string, string) result
  = fun src_dpkdir ->
  let len = String.length src_dpkdir in
  let last_pos = len - if src_dpkdir.[len - 1] = '/' then 2 else 1 in
  let start_pos = match String.rindex_from_opt src_dpkdir last_pos '/' with
    | None -> 0
    | Some n -> n in
  let* n = String.index_from_opt src_dpkdir start_pos '_'
         |> Option.to_result ~none:"invalid dpkdir name" in
  let name_with_prefix = String.sub src_dpkdir start_pos (n - start_pos) in
  match String.sub name_with_prefix 0 4 with
  | "map-" -> Ok (String.sub name_with_prefix 4 (String.length name_with_prefix - 4))
  | _ -> Error "dpkdir must start with map-"

let main : string -> string -> (unit, string) result
  = fun input_path output_path ->
  let open_out_result path = try Ok (open_out path) with Sys_error s -> Error s in
  let* map_name = map_name_of_dpkdir output_path in
  let map_source_path = output_path ^ "/maps/" ^ map_name ^ ".map" in
  let* lines = input_lines input_path in
  let* lines = eat_option_lines lines in
  let arr = parse_input lines in
  let (_, _, dim_z) = array3_dim arr in
  if dim_z = 1 then cfg_ladders := false;
  let brushes = compile_ascii_art arr in
  let brushes = if !cfg_single_sky then create_single_sky arr :: brushes else brushes in
  let* stream = open_out_result map_source_path in
  write_map brushes stream;
  write_intermission arr stream;
  write_buildings arr stream;
  close_out stream;
  let rec loop = function
    | (classname, down_max, up_max, pounces_up, uses_arm) :: rest ->
       let* stream = open_out_result (output_path ^ "/maps/" ^ map_name ^ "-" ^ classname ^ ".navcon") in
       write_navcons arr down_max up_max pounces_up uses_arm stream;
       close_out stream;
       loop rest
    | [] -> Ok () in
  loop [
      ("builder", max_int, 0, false, false);
      ("builderupg", max_int, max_int, false, false);
      ("level0", max_int, max_int, false, false);
      ("level1", max_int, max_int, false, false);
      ("level2", max_int, 256, false, false);
      ("level2upg", max_int, 256, false, false);
      ("level3", max_int, 256, true, false);
      ("level3upg", max_int, 390, true, false);
      ("level4", max_int, 0, false, false);
      ("human_naked", 256, max_int, false, true);
      ("human_bsuit", 512, max_int, false, true);
    ]

let main_cmdline () =
  Random.self_init ();
  match Sys.argv with
  | [| own_name; input_path; output_path |] ->
     begin
       match main input_path output_path with
       | Error s -> Printf.printf "Error:\n  %s\n" s
       | Ok () -> ()
     end
  | arr ->
     Printf.printf "Usage:\n    %s <ascii-art-file> <map-file>\n" arr.(0)

let () = main_cmdline ()
