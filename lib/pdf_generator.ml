open Font_utils
open Label_layouts

let save_pdf_to_file pdf filename = try Pdfwrite.pdf_to_file pdf filename with e -> failwith ("Failed to save PDF: " ^ Printexc.to_string e)

(* Text wrapping functionality *)
let estimate_text_width font_size widths text =
  let char_width_sum =
    String.fold_left
      (fun acc c ->
        let char_code = int_of_char c in
        let char_width =
          if char_code >= 32 && char_code <= 126 then widths.(char_code) else widths.(32)
          (* space width as fallback *)
        in
        acc +. char_width)
      0.0 text
  in
  char_width_sum *. font_size /. 1000.0

let wrap_text ?(max_width_with_checkbox = 0.0) ?(checkbox_height = 0.0) ?(line_height = 0.0) text max_width font_size widths =
  (* First split on explicit line breaks *)
  let user_lines = String.split_on_char '\n' text in

  let all_lines, _ =
    List.fold_left
      (fun (all_lines, current_y) user_line ->
        let words = String.split_on_char ' ' user_line in

        let lines, final_y =
          List.fold_left
            (fun (lines, line_y) word ->
              let current_line = List.hd lines in
              let line = current_line @ [ word ] in
              let line_max_width = if line_y <= checkbox_height then max_width_with_checkbox else max_width in
              let test_width = String.concat " " line |> estimate_text_width font_size widths in
              if test_width <= line_max_width then (line :: List.tl lines, line_y)
              else if List.is_empty current_line then
                (* Single word is too long, just add it anyway *)
                ([ word ] :: List.tl lines, line_y)
              else ([ word ] :: lines, line_y +. line_height))
            ([ [] ], current_y) words
        in
        let processed_lines = List.rev_map (String.concat " ") lines in
        (* Add empty string before this user_line (except for the first one) to create gap after manual newlines *)
        let lines_with_gap = if all_lines = [] then processed_lines else "" :: processed_lines in
        (all_lines @ lines_with_gap, final_y +. (line_height *. 1.5)))
      ([], 0.0) user_lines
  in

  all_lines

let create_pdf_with_labels font_bytes text layout_name font_size ?(show_borders = false) ?(include_checkbox = true) () =
  try
    (* Get the selected layout *)
    let layout =
      match get_layout_by_name layout_name with Some l -> l | None -> avery_l7160
      (* default fallback *)
    in

    (* Extract basic font data and proper character mappings *)
    let ps_name, bbox, ascent, descent, _ = extract_basic_font_data font_bytes in
    let char_mappings, widths, _scale_factor = extract_cmap_and_widths font_bytes in

    let pdf = Pdf.empty () in

    (* Add catalog *)
    let catalog_dict = Pdf.Dictionary [ ("/Type", Pdf.Name "/Catalog") ] in
    let catalog_num = Pdf.addobj pdf catalog_dict in
    let pdf = { pdf with Pdf.root = catalog_num } in

    (* Add pages tree *)
    let pages_dict = Pdf.Dictionary [ ("/Type", Pdf.Name "/Pages"); ("/Kids", Pdf.Array []); ("/Count", Pdf.Integer 0) ] in
    let pages_num = Pdf.addobj pdf pages_dict in

    (* Update catalog *)
    let updated_catalog = Pdf.Dictionary [ ("/Type", Pdf.Name "/Catalog"); ("/Pages", Pdf.Indirect pages_num) ] in
    Pdf.addobj_given_num pdf (catalog_num, updated_catalog);

    (* Embed font *)
    let font_data_stream =
      Pdf.Stream
        (ref
           ( Pdf.Dictionary [ ("/Length", Pdf.Integer (Bytes.length font_bytes)); ("/Length1", Pdf.Integer (Bytes.length font_bytes)) ],
             Pdf.Got (Pdfio.bytes_of_string (Bytes.to_string font_bytes)) ))
    in
    let font_data_num = Pdf.addobj pdf font_data_stream in

    (* Create proper ToUnicode CMap with actual character mappings *)
    let subset_name = "CUSTOM+" ^ ps_name in
    let cmap_content = create_proper_cmap char_mappings subset_name in
    let cmap_stream =
      Pdf.Stream (ref (Pdf.Dictionary [ ("/Length", Pdf.Integer (String.length cmap_content)) ], Pdf.Got (Pdfio.bytes_of_string cmap_content)))
    in
    let cmap_num = Pdf.addobj pdf cmap_stream in

    (* Font descriptor with extracted metrics *)
    let bbox_left, bbox_bottom, bbox_right, bbox_top = bbox in
    let cap_height = ascent *. 0.8 in
    (* estimate *)
    let missing_width = 600.0 in
    (* reasonable default *)
    let stem_v = 100.0 in
    (* reasonable default *)

    let font_descriptor =
      Pdf.Dictionary
        [
          ("/Type", Pdf.Name "/FontDescriptor");
          ("/FontName", Pdf.Name ("/" ^ subset_name));
          ("/FontFile2", Pdf.Indirect font_data_num);
          ("/Flags", Pdf.Integer 4);
          (* Non-symbolic *)
          ("/FontBBox", Pdf.Array [ Pdf.Real bbox_left; Pdf.Real bbox_bottom; Pdf.Real bbox_right; Pdf.Real bbox_top ]);
          ("/ItalicAngle", Pdf.Real 0.0);
          ("/Ascent", Pdf.Real ascent);
          ("/Descent", Pdf.Real descent);
          ("/CapHeight", Pdf.Real cap_height);
          ("/StemV", Pdf.Real stem_v);
          ("/MissingWidth", Pdf.Real missing_width);
        ]
    in
    let font_descriptor_num = Pdf.addobj pdf font_descriptor in

    (* Create font with proper character range and actual widths *)
    let first_char = 32 in
    (* Space character *)
    let last_char = 126 in
    (* Tilde character *)
    let width_array = Array.sub widths first_char (last_char - first_char + 1) in

    let font_dict =
      Pdf.Dictionary
        [
          ("/Type", Pdf.Name "/Font");
          ("/Subtype", Pdf.Name "/TrueType");
          ("/BaseFont", Pdf.Name ("/" ^ subset_name));
          ("/FontDescriptor", Pdf.Indirect font_descriptor_num);
          ("/FirstChar", Pdf.Integer first_char);
          ("/LastChar", Pdf.Integer last_char);
          ("/Widths", Pdf.Array (Array.to_list (Array.map (fun w -> Pdf.Real w) width_array)));
          ("/ToUnicode", Pdf.Indirect cmap_num);
          ("/Encoding", Pdf.Name "/WinAnsiEncoding") (* Standard encoding *);
        ]
    in
    let font_num = Pdf.addobj pdf font_dict in

    (* Generate content stream for all labels *)
    let positions = get_all_label_positions layout in

    (* Calculate text wrapping parameters *)
    let label_width_points = mm_to_points layout.label_width_mm in
    let label_height_points = mm_to_points layout.label_height_mm in
    let text_margin = 3.0 in
    (* 3 points margin inside label *)
    let checkbox_height = font_size in
    (* checkbox matches font size *)
    let checkbox_margin = 5.0 in
    (* 5 points margin from edge for better clearance *)
    let line_height = font_size *. 1.2 in
    (* 120% of font size *)

    (* Calculate text widths - checkbox only affects top lines *)
    let max_width = label_width_points -. (2.0 *. text_margin) in
    let max_width_with_checkbox =
      if checkbox_height > 0.0 then label_width_points -. (2.0 *. text_margin) -. checkbox_height -. (checkbox_margin -. text_margin) else max_width
    in

    (* Wrap text into lines with checkbox awareness *)
    let text_lines = wrap_text ~max_width_with_checkbox ~checkbox_height ~line_height text max_width font_size widths in

    let content_parts =
      List.map
        (fun (x, y) ->
          let border_parts = if show_borders then [ Printf.sprintf "%.2f %.2f %.2f %.2f re S" x y label_width_points label_height_points ] else [] in

          let checkbox_parts =
            if include_checkbox then
              let checkbox_x = x +. label_width_points -. checkbox_height -. checkbox_margin in
              let checkbox_y = y +. label_height_points -. checkbox_height -. checkbox_margin in
              [ Printf.sprintf "%.2f %.2f %.2f %.2f re S" checkbox_x checkbox_y checkbox_height checkbox_height ]
            else []
          in

          let text_parts =
            let _, parts = List.fold_left
              (fun (current_y, parts) line ->
                if line = "" then
                  (* Empty line represents gap after manual newline - use smaller spacing *)
                  (current_y +. (line_height /. 2.0), parts)
                else
                  let text_x = x +. text_margin in
                  let text_y = y +. label_height_points -. text_margin -. current_y -. font_size in
                  let text_part = Printf.sprintf "BT /F1 %.1f Tf %.2f %.2f Td (%s) Tj ET" font_size text_x text_y line in
                  (current_y +. line_height, text_part :: parts))
              (0.0, []) text_lines
            in
            List.rev parts
          in

          let all_parts = border_parts @ checkbox_parts @ text_parts in
          String.concat "\n" all_parts)
        positions
    in

    let full_content = String.concat "\n" content_parts in

    (* Content stream *)
    let content_dict = Pdf.Dictionary [ ("/Length", Pdf.Integer (String.length full_content)) ] in
    let content_stream = Pdf.Stream (ref (content_dict, Pdf.Got (Pdfio.bytes_of_string full_content))) in
    let content_num = Pdf.addobj pdf content_stream in

    (* Page *)
    let page_width = mm_to_points a4_width_mm in
    let page_height = mm_to_points a4_height_mm in

    let page_dict =
      Pdf.Dictionary
        [
          ("/Type", Pdf.Name "/Page");
          ("/Parent", Pdf.Indirect pages_num);
          ("/MediaBox", Pdf.Array [ Pdf.Real 0.0; Pdf.Real 0.0; Pdf.Real page_width; Pdf.Real page_height ]);
          ("/Contents", Pdf.Indirect content_num);
          ("/Resources", Pdf.Dictionary [ ("/Font", Pdf.Dictionary [ ("/F1", Pdf.Indirect font_num) ]) ]);
        ]
    in
    let page_num = Pdf.addobj pdf page_dict in

    (* Update pages tree *)
    let updated_pages = Pdf.Dictionary [ ("/Type", Pdf.Name "/Pages"); ("/Kids", Pdf.Array [ Pdf.Indirect page_num ]); ("/Count", Pdf.Integer 1) ] in
    Pdf.addobj_given_num pdf (pages_num, updated_pages);

    pdf
  with e -> failwith ("Multi-label PDF generation failed: " ^ Printexc.to_string e)

let create_pdf_as_string pdf =
  try
    let output, bytes_ref = Pdfio.input_output_of_bytes 16384 in
    Pdfwrite.pdf_to_output None true pdf output;
    let pdf_bytes = Pdfio.extract_bytes_from_input_output output bytes_ref in

    (* Convert PDF bytes to string for download *)
    let pdf_string = Pdfio.string_of_bytes pdf_bytes in
    pdf_string
  with e -> "Error: " ^ Printexc.to_string e
