open Label_maker_lib.Pdf_generator
open Label_maker_lib.Label_layouts
open Printf

let test_label_layout layout_name =
  printf "=== Testing %s Layout ===\n%!" layout_name;

  match get_layout_by_name layout_name with
  | Some layout ->
      print_layout_info layout;
      let positions = get_all_label_positions layout in
      printf "Label positions:\n%!";
      List.iteri (fun i (x, y) -> printf "  Label %d: (%.1f, %.1f) points\n%!" (i + 1) x y) positions;
      printf "\n%!"
  | None -> printf "ERROR: Layout '%s' not found!\n%!" layout_name

let () =
  printf "=== Avery Label Layout Test ===\n\n%!";

  test_label_layout "Avery L7160";
  test_label_layout "Avery L7162";
  test_label_layout "Avery L7160-93";

  printf "=== Testing Label Generation ===\n%!";

  (* Load font file *)
  let font_file = "XCCW_Joined_23a.ttf" in
  if Sys.file_exists font_file then (
    let ic = open_in_bin font_file in
    let length = in_channel_length ic in
    let font_bytes = Bytes.create length in
    really_input ic font_bytes 0 length;
    close_in ic;

    printf "Font loaded: %d bytes\n%!" length;

    (* Test generating PDFs for each layout *)
    List.iter
      (fun layout ->
        try
          printf "Generating test PDF for %s...\n%!" layout.name;
          let pdf =
            create_pdf_with_labels font_bytes
              "Line 1: This is a longer test\nLine 2: Text with explicit line breaks\nLine 3: And automatic wrapping functionality" layout.name 12.0
              ~show_borders:true ~include_checkbox:true ()
          in
          let filename = "test_" ^ String.lowercase_ascii (String.map (function ' ' -> '_' | c -> c) layout.name) ^ "_full_features.pdf" in
          save_pdf_to_file pdf filename;
          printf "Saved: %s\n%!" filename
        with e -> printf "ERROR generating %s: %s\n%!" layout.name (Printexc.to_string e))
      available_layouts)
  else printf "Font file '%s' not found. Skipping PDF generation test.\n%!" font_file
