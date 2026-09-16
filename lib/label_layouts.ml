(* Standard A4 dimensions in mm *)
let a4_width_mm = 210.0
let a4_height_mm = 297.0

(* Convert mm to PDF points (1 mm = 2.834645669 points) *)
let mm_to_points mm = mm *. 2.834645669
let points_to_mm pt = pt /. 2.834645669

type label_layout = {
  name : string;
  label_width_mm : float;
  label_height_mm : float;
  cols : int;
  rows : int;
  margin_left_mm : float;
  margin_top_mm : float;
  spacing_x_mm : float;
  spacing_y_mm : float;
}

(* Avery label specifications *)
let avery_l7160 =
  {
    name = "Avery L7160";
    label_width_mm = 63.5;
    label_height_mm = 38.1;
    cols = 3;
    rows = 7;
    margin_left_mm = 7.0;
    (* estimated based on standard A4 margins *)
    margin_top_mm = 15.0;
    (* estimated based on standard A4 margins *)
    spacing_x_mm = 2.5;
    (* estimated spacing between labels *)
    spacing_y_mm = 0.0;
    (* estimated spacing between rows *)
  }

let avery_l7162 =
  {
    name = "Avery L7162";
    label_width_mm = 99.1;
    label_height_mm = 33.9;
    cols = 2;
    rows = 8;
    margin_left_mm = 6.0;
    (* estimated based on standard A4 margins *)
    margin_top_mm = 15.0;
    (* estimated based on standard A4 margins *)
    spacing_x_mm = 0.0;
    (* estimated spacing between labels *)
    spacing_y_mm = 0.0;
    (* estimated spacing between rows *)
  }

let available_layouts = [ avery_l7160; avery_l7162 ]

(* Smallest page box, in mm as (x0, y0, x1, y1) with PDF origin bottom-left,
   that contains the given ink bounding box AND stays concentric with A4: the
   same inset is taken off both sides of each axis, using whichever side has
   less clearance.

   Staying concentric matters because a driver handed a smaller-than-A4 page
   centres it on the sheet, so keeping the page centre on the A4 centre puts
   every label back at its true position. Cropping tight to the ink instead
   would offset the sheet by half the difference between opposite margins.

   The box is measured against what is actually drawn, not against the label
   grid, because the grid runs closer to the paper edge than the ink does: on
   a printer whose unprintable margin exceeds the grid margin, cropping to the
   grid still leaves an oversized page for the driver to shrink. Drawn
   coordinates are unaffected either way -- they stay in full-A4 space and the
   box simply crops away the blank margin around them. *)
let max_crop_inset_mm = 15.0

let concentric_page_box_mm (ink_x0, ink_y0, ink_x1, ink_y1) =
  (* Never crop further than [max_crop_inset_mm]. Centred or right-aligned
     short text leaves the ink far from the paper edge, and cropping to it
     would hand the driver a page a fraction of A4: harmless if the driver
     only shrinks oversized pages, but a driver that scales every page to fit
     would blow such a page up. No printer needs a deeper crop than this. *)
  let inset side_a side_b = Float.min max_crop_inset_mm (Float.max 0.0 (Float.min side_a side_b)) in
  let x_inset = inset ink_x0 (a4_width_mm -. ink_x1) in
  let y_inset = inset ink_y0 (a4_height_mm -. ink_y1) in
  (x_inset, y_inset, a4_width_mm -. x_inset, a4_height_mm -. y_inset)

let full_page_box_mm = (0.0, 0.0, a4_width_mm, a4_height_mm)

(* Bounding box of the label grid itself, in mm. Only the label edges, so it is
   wider than the ink; used for reporting rather than for the page box. *)
let label_block_box_mm layout =
  let block_width = (float_of_int layout.cols *. layout.label_width_mm) +. (float_of_int (layout.cols - 1) *. layout.spacing_x_mm) in
  let block_height = (float_of_int layout.rows *. layout.label_height_mm) +. (float_of_int (layout.rows - 1) *. layout.spacing_y_mm) in
  (layout.margin_left_mm, a4_height_mm -. (layout.margin_top_mm +. block_height), layout.margin_left_mm +. block_width, a4_height_mm -. layout.margin_top_mm)

(* Calculate label position for given row and column *)
let calculate_label_position layout row col =
  let x_mm = layout.margin_left_mm +. (float_of_int col *. (layout.label_width_mm +. layout.spacing_x_mm)) in
  (* PDF coordinates start from bottom-left, so we need to flip Y *)
  let y_from_top_mm = layout.margin_top_mm +. (float_of_int row *. (layout.label_height_mm +. layout.spacing_y_mm)) in
  let y_mm = a4_height_mm -. y_from_top_mm -. layout.label_height_mm in
  (mm_to_points x_mm, mm_to_points y_mm)

(* Get all label positions for a layout *)
let get_all_label_positions layout =
  let positions = ref [] in
  for row = 0 to layout.rows - 1 do
    for col = 0 to layout.cols - 1 do
      let x, y = calculate_label_position layout row col in
      positions := (x, y) :: !positions
    done
  done;
  List.rev !positions

(* Calculate optimal font size to fit text in label *)
let calculate_optimal_font_size layout text max_font_size =
  let label_width_points = mm_to_points layout.label_width_mm in
  let label_height_points = mm_to_points layout.label_height_mm in

  (* Rough estimation: assume average character width is 0.6 * font_size *)
  let text_length = String.length text in
  let estimated_width_per_point = 0.6 in

  let max_font_size_for_width = if text_length > 0 then label_width_points /. (float_of_int text_length *. estimated_width_per_point) else max_font_size in

  (* Limit height to about 80% of label height *)
  let max_font_size_for_height = label_height_points *. 0.8 in

  min max_font_size (min max_font_size_for_width max_font_size_for_height)

(* Get layout by name *)
let get_layout_by_name name = List.find_opt (fun layout -> layout.name = name) available_layouts

(* Debug function to print layout information *)
let print_layout_info layout =
  let open Printf in
  printf "Layout: %s\n" layout.name;
  printf "Label size: %.1f x %.1f mm\n" layout.label_width_mm layout.label_height_mm;
  printf "Grid: %d cols x %d rows = %d labels\n" layout.cols layout.rows (layout.cols * layout.rows);
  printf "Margins: left=%.1f mm, top=%.1f mm\n" layout.margin_left_mm layout.margin_top_mm;
  printf "Spacing: x=%.1f mm, y=%.1f mm\n" layout.spacing_x_mm layout.spacing_y_mm;
  let x0, y0, x1, y1 = concentric_page_box_mm (label_block_box_mm layout) in
  printf "Page box if cropped to the labels: [%.1f %.1f %.1f %.1f] mm = %.1f x %.1f mm (%.1f%% x %.1f%% of A4)\n" x0 y0 x1 y1 (x1 -. x0) (y1 -. y0)
    (100.0 *. (x1 -. x0) /. a4_width_mm)
    (100.0 *. (y1 -. y0) /. a4_height_mm);
  printf "   (the real page box is cropped to the ink, which is narrower still)\n"
