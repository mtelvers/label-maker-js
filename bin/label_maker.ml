(* simplified js_of_ocaml version using the library *)
open Js_of_ocaml
open Label_maker_lib.Pdf_generator

let log_message msg = Firebug.console##log (Js.string msg)

let justification_of_string = function
  | "Left" -> Label_maker_lib.Pdf_generator.Left
  | "Center" -> Label_maker_lib.Pdf_generator.Center
  | "Right" -> Label_maker_lib.Pdf_generator.Right
  | "Justify" -> Label_maker_lib.Pdf_generator.Justify
  | _ -> Label_maker_lib.Pdf_generator.Left (* default fallback *)

(* The fonts baked into the js_of_ocaml virtual filesystem by bin/dune.
   [expected_size] is a tripwire for a bundle truncated in transit. *)
type label_font = { key : string; menu_label : string; file : string; expected_size : int }

let cursive_font = { key = "cursive"; menu_label = "Cursive (XCCW Joined)"; file = "XCCW_Joined_23a.ttf"; expected_size = 63200 }

(* Comic Relief stands in for Comic Sans, which cannot be redistributed. *)
let comic_font = { key = "comic"; menu_label = "Comic Sans style (Comic Relief)"; file = "ComicRelief-Regular.ttf"; expected_size = 80324 }

(* Cursive stays first so it remains the default selection. *)
let available_fonts = [ cursive_font; comic_font ]
let font_of_key key = match List.find_opt (fun f -> f.key = key) available_fonts with Some f -> f | None -> cursive_font

(* Returns a description of the problem, or None if the font looks intact.
   Only called for its diagnostics, so it must never raise. *)
let font_data_problem font font_bytes =
  let actual_size = Bytes.length font_bytes in
  let has_font_magic =
    actual_size >= 4
    &&
    let magic = Printf.sprintf "%c%c%c%c" (Bytes.get font_bytes 0) (Bytes.get font_bytes 1) (Bytes.get font_bytes 2) (Bytes.get font_bytes 3) in
    magic = "\x00\x01\x00\x00" || magic = "OTTO" || magic = "true" || magic = "ttcf"
  in
  if not has_font_magic then Some (font.file ^ " does not start with a TrueType/OpenType signature")
  else if actual_size <> font.expected_size then Some (Printf.sprintf "%s is %d bytes, expected %d" font.file actual_size font.expected_size)
  else None

let load_font_from_fs font =
  try
    let ic = open_in_bin font.file in
    let length = in_channel_length ic in
    let bytes = Bytes.create length in
    really_input ic bytes 0 length;
    close_in ic;
    (match font_data_problem font bytes with Some problem -> log_message ("WARNING: " ^ problem) | None -> ());
    Some bytes
  with e ->
    log_message ("Font loading error for " ^ font.file ^ ": " ^ Printexc.to_string e);
    None

let download_pdf_binary_safe pdf_content filename =
  log_message (Printf.sprintf "Opening %s (%d bytes)" filename (String.length pdf_content));

  (* Convert string to Uint8Array for proper binary handling *)
  let length = String.length pdf_content in
  let uint8_array = new%js Typed_array.uint8Array length in

  for i = 0 to length - 1 do
    Typed_array.set uint8_array i (int_of_char (String.get pdf_content i))
  done;

  (* Create blob from Uint8Array *)
  let blob_constructor = Js.Unsafe.js_expr "Blob" in
  let blob_data = Js.array [| uint8_array |] in
  let blob_options =
    object%js
      val type_ = Js.string "application/pdf"
    end
  in
  let blob = Js.Unsafe.new_obj blob_constructor [| Js.Unsafe.inject blob_data; Js.Unsafe.inject blob_options |] in
  let url = Js.Unsafe.fun_call (Js.Unsafe.js_expr "URL.createObjectURL") [| Js.Unsafe.inject blob |] in

  let a = Dom_html.createA Dom_html.document in
  a##.href := url;
  (* Remove download attribute to open PDF in browser instead of downloading *)
  a##.style##.display := Js.string "none";

  let body = Dom_html.document##.body in
  Dom.appendChild body a;
  a##click;
  Dom.removeChild body a;
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "URL.revokeObjectURL") [| url |])

let download_pdf pdf_content filename =
  try download_pdf_binary_safe pdf_content filename
  with e ->
    log_message ("Binary-safe download failed, falling back to string method: " ^ Printexc.to_string e);
    let blob_constructor = Js.Unsafe.js_expr "Blob" in
    let blob_data = Js.array [| Js.string pdf_content |] in
    let blob_options =
      object%js
        val type_ = Js.string "application/pdf"
      end
    in
    let blob = Js.Unsafe.new_obj blob_constructor [| Js.Unsafe.inject blob_data; Js.Unsafe.inject blob_options |] in
    let url = Js.Unsafe.fun_call (Js.Unsafe.js_expr "URL.createObjectURL") [| Js.Unsafe.inject blob |] in

    let a = Dom_html.createA Dom_html.document in
    a##.href := url;
    (* Remove download attribute to open PDF in browser instead of downloading *)
    a##.style##.display := Js.string "none";

    let body = Dom_html.document##.body in
    Dom.appendChild body a;
    a##click;
    Dom.removeChild body a;
    ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "URL.revokeObjectURL") [| url |])

let create_textarea () =
  let textarea = Dom_html.createTextarea Dom_html.document in
  textarea

let create_select_option value text =
  let option = Dom_html.createOption Dom_html.document in
  option##.value := Js.string value;
  option##.innerHTML := Js.string text;
  option

let () =
  let body = Dom_html.document##.body in

  (* Set body styling for centering *)
  body##.style##.margin := Js.string "0";
  body##.style##.padding := Js.string "40px 20px";
  body##.style##.fontFamily := Js.string "Arial, sans-serif";
  body##.style##.backgroundColor := Js.string "#f5f5f5";

  (* Main container *)
  let main_container = Dom_html.createDiv Dom_html.document in
  main_container##.style##.maxWidth := Js.string "600px";
  main_container##.style##.margin := Js.string "0 auto";
  main_container##.style##.backgroundColor := Js.string "white";
  main_container##.style##.padding := Js.string "40px";
  main_container##.style##.borderRadius := Js.string "8px";
  main_container##.style##.border := Js.string "1px solid #ddd";

  (* Title *)
  let title = Dom_html.createH1 Dom_html.document in
  title##.innerHTML := Js.string "Avery Label Maker";
  title##.style##.textAlign := Js.string "center";
  title##.style##.marginBottom := Js.string "30px";
  title##.style##.color := Js.string "#333";
  Dom.appendChild main_container title;

  (* Form container *)
  let form_div = Dom_html.createDiv Dom_html.document in

  (* Text input *)
  let text_label = Dom_html.createLabel Dom_html.document in
  text_label##.innerHTML := Js.string "Text for labels:";
  text_label##.style##.display := Js.string "block";
  text_label##.style##.marginBottom := Js.string "8px";
  text_label##.style##.fontWeight := Js.string "bold";
  text_label##.style##.color := Js.string "#555";
  Dom.appendChild form_div text_label;

  let text_input = create_textarea () in
  text_input##.value := Js.string "Date\nLO";
  text_input##.style##.width := Js.string "100%";
  text_input##.style##.height := Js.string "80px";
  text_input##.style##.marginBottom := Js.string "20px";
  text_input##.style##.padding := Js.string "10px";
  text_input##.style##.border := Js.string "1px solid #ddd";
  text_input##.style##.borderRadius := Js.string "4px";
  text_input##.style##.fontFamily := Js.string "Arial, sans-serif";
  text_input##.style##.fontSize := Js.string "14px";
  Dom.appendChild form_div text_input;

  (* Layout selection *)
  let layout_label = Dom_html.createLabel Dom_html.document in
  layout_label##.innerHTML := Js.string "Label Layout:";
  layout_label##.style##.display := Js.string "block";
  layout_label##.style##.marginBottom := Js.string "8px";
  layout_label##.style##.fontWeight := Js.string "bold";
  layout_label##.style##.color := Js.string "#555";
  Dom.appendChild form_div layout_label;

  let layout_select = Dom_html.createSelect Dom_html.document in
  layout_select##.style##.width := Js.string "100%";
  layout_select##.style##.padding := Js.string "10px";
  layout_select##.style##.marginBottom := Js.string "20px";
  layout_select##.style##.border := Js.string "1px solid #ddd";
  layout_select##.style##.borderRadius := Js.string "4px";
  layout_select##.style##.fontSize := Js.string "14px";

  let option_l7160 = create_select_option "Avery L7160" "Avery L7160 (21 labels, 63.5x38.1mm)" in
  let option_l7162 = create_select_option "Avery L7162" "Avery L7162 (16 labels, 99.1x33.9mm)" in
  let option_l7160_93 = create_select_option "Avery L7160-93" "Avery L7160-93 (21 labels, 63.5x38.1mm)" in

  Dom.appendChild layout_select option_l7160;
  Dom.appendChild layout_select option_l7162;
  Dom.appendChild layout_select option_l7160_93;
  Dom.appendChild form_div layout_select;

  (* Font selection *)
  let font_label = Dom_html.createLabel Dom_html.document in
  font_label##.innerHTML := Js.string "Font:";
  font_label##.style##.display := Js.string "block";
  font_label##.style##.marginBottom := Js.string "8px";
  font_label##.style##.fontWeight := Js.string "bold";
  font_label##.style##.color := Js.string "#555";
  Dom.appendChild form_div font_label;

  let font_select = Dom_html.createSelect Dom_html.document in
  font_select##.style##.width := Js.string "100%";
  font_select##.style##.padding := Js.string "10px";
  font_select##.style##.marginBottom := Js.string "20px";
  font_select##.style##.border := Js.string "1px solid #ddd";
  font_select##.style##.borderRadius := Js.string "4px";
  font_select##.style##.fontSize := Js.string "14px";

  List.iter (fun f -> Dom.appendChild font_select (create_select_option f.key f.menu_label)) available_fonts;
  Dom.appendChild form_div font_select;

  (* Font size input *)
  let font_size_label = Dom_html.createLabel Dom_html.document in
  font_size_label##.innerHTML := Js.string "Font Size:";
  font_size_label##.style##.display := Js.string "block";
  font_size_label##.style##.marginBottom := Js.string "8px";
  font_size_label##.style##.fontWeight := Js.string "bold";
  font_size_label##.style##.color := Js.string "#555";
  Dom.appendChild form_div font_size_label;

  let font_size_container = Dom_html.createDiv Dom_html.document in
  font_size_container##.style##.marginBottom := Js.string "20px";

  let font_size_input = Dom_html.createInput ~_type:(Js.string "number") Dom_html.document in
  font_size_input##.value := Js.string "12";
  font_size_input##setAttribute (Js.string "min") (Js.string "6");
  font_size_input##setAttribute (Js.string "max") (Js.string "72");
  font_size_input##.style##.width := Js.string "80px";
  font_size_input##.style##.padding := Js.string "10px";
  font_size_input##.style##.border := Js.string "1px solid #ddd";
  font_size_input##.style##.borderRadius := Js.string "4px";
  font_size_input##.style##.fontSize := Js.string "14px";
  Dom.appendChild font_size_container font_size_input;

  let font_size_unit = Dom_html.createSpan Dom_html.document in
  font_size_unit##.innerHTML := Js.string " pt";
  font_size_unit##.style##.marginLeft := Js.string "8px";
  font_size_unit##.style##.color := Js.string "#666";
  Dom.appendChild font_size_container font_size_unit;

  Dom.appendChild form_div font_size_container;

  (* Text Justification selection *)
  let justification_label = Dom_html.createLabel Dom_html.document in
  justification_label##.innerHTML := Js.string "Text Alignment:";
  justification_label##.style##.display := Js.string "block";
  justification_label##.style##.marginBottom := Js.string "8px";
  justification_label##.style##.fontWeight := Js.string "bold";
  justification_label##.style##.color := Js.string "#555";
  Dom.appendChild form_div justification_label;

  let justification_select = Dom_html.createSelect Dom_html.document in
  justification_select##.style##.width := Js.string "100%";
  justification_select##.style##.padding := Js.string "10px";
  justification_select##.style##.marginBottom := Js.string "20px";
  justification_select##.style##.border := Js.string "1px solid #ddd";
  justification_select##.style##.borderRadius := Js.string "4px";
  justification_select##.style##.fontSize := Js.string "14px";

  let option_left = create_select_option "Left" "Left aligned" in
  let option_center = create_select_option "Center" "Center aligned" in
  let option_right = create_select_option "Right" "Right aligned" in
  let option_justify = create_select_option "Justify" "Fully justified" in

  Dom.appendChild justification_select option_left;
  Dom.appendChild justification_select option_center;
  Dom.appendChild justification_select option_right;
  Dom.appendChild justification_select option_justify;
  Dom.appendChild form_div justification_select;

  (* Options section *)
  let options_container = Dom_html.createDiv Dom_html.document in
  options_container##.style##.marginBottom := Js.string "20px";

  (* Border checkbox *)
  let border_container = Dom_html.createDiv Dom_html.document in
  border_container##.style##.marginBottom := Js.string "10px";

  let border_checkbox = Dom_html.createInput ~_type:(Js.string "checkbox") Dom_html.document in
  border_checkbox##.style##.marginRight := Js.string "8px";
  Dom.appendChild border_container border_checkbox;

  let border_label = Dom_html.createLabel Dom_html.document in
  border_label##.innerHTML := Js.string "Show borders (debug)";
  border_label##.style##.fontSize := Js.string "14px";
  border_label##.style##.color := Js.string "#555";
  Dom.appendChild border_container border_label;

  let border_help = Dom_html.createSpan Dom_html.document in
  border_help##.innerHTML := Js.string " - helps visualize label positioning";
  border_help##.style##.fontSize := Js.string "12px";
  border_help##.style##.color := Js.string "#888";
  Dom.appendChild border_container border_help;

  Dom.appendChild options_container border_container;

  (* Checkbox feature *)
  let checkbox_container = Dom_html.createDiv Dom_html.document in
  checkbox_container##.style##.marginBottom := Js.string "10px";

  let checkbox_feature_checkbox = Dom_html.createInput ~_type:(Js.string "checkbox") Dom_html.document in
  checkbox_feature_checkbox##.checked := Js.bool true;
  checkbox_feature_checkbox##.style##.marginRight := Js.string "8px";
  Dom.appendChild checkbox_container checkbox_feature_checkbox;

  let checkbox_feature_label = Dom_html.createLabel Dom_html.document in
  checkbox_feature_label##.innerHTML := Js.string "Include checkbox";
  checkbox_feature_label##.style##.fontSize := Js.string "14px";
  checkbox_feature_label##.style##.color := Js.string "#555";
  Dom.appendChild checkbox_container checkbox_feature_label;

  let checkbox_feature_help = Dom_html.createSpan Dom_html.document in
  checkbox_feature_help##.innerHTML := Js.string " - adds 8mm square for ticking";
  checkbox_feature_help##.style##.fontSize := Js.string "12px";
  checkbox_feature_help##.style##.color := Js.string "#888";
  Dom.appendChild checkbox_container checkbox_feature_help;

  Dom.appendChild options_container checkbox_container;
  Dom.appendChild form_div options_container;

  (* Generate button *)
  let button_container = Dom_html.createDiv Dom_html.document in
  button_container##.style##.textAlign := Js.string "center";
  button_container##.style##.marginTop := Js.string "30px";

  let generate_button = Dom_html.createButton Dom_html.document in
  generate_button##.innerHTML := Js.string "Generate Label PDF";
  generate_button##.style##.backgroundColor := Js.string "#4CAF50";
  generate_button##.style##.color := Js.string "white";
  generate_button##.style##.padding := Js.string "12px 24px";
  generate_button##.style##.border := Js.string "none";
  generate_button##.style##.borderRadius := Js.string "4px";
  generate_button##.style##.cursor := Js.string "pointer";
  generate_button##.style##.fontSize := Js.string "16px";
  generate_button##.style##.fontWeight := Js.string "500";

  Dom.appendChild button_container generate_button;

  (* Status area: failures used to be swallowed silently, leaving the button
     looking like it had done nothing. *)
  let status_div = Dom_html.createDiv Dom_html.document in
  status_div##.style##.marginTop := Js.string "20px";
  status_div##.style##.padding := Js.string "12px";
  status_div##.style##.borderRadius := Js.string "4px";
  status_div##.style##.fontSize := Js.string "14px";
  status_div##.style##.textAlign := Js.string "left";
  status_div##.style##.display := Js.string "none";
  Dom.appendChild button_container status_div;

  let clear_status () = status_div##.style##.display := Js.string "none" in
  let set_status ~background ~border ~color msg =
    status_div##.textContent := Js.some (Js.string msg);
    status_div##.style##.backgroundColor := Js.string background;
    status_div##.style##.border := Js.string ("1px solid " ^ border);
    status_div##.style##.color := Js.string color;
    status_div##.style##.display := Js.string "block"
  in
  let show_error msg =
    log_message ("ERROR: " ^ msg);
    set_status ~background:"#fdecea" ~border:"#f5c2c0" ~color:"#611a15" msg
  in
  let show_info msg =
    log_message msg;
    set_status ~background:"#edf7ed" ~border:"#c5e1c5" ~color:"#1e4620" msg
  in

  generate_button##.onclick :=
    Dom_html.handler (fun _ ->
        (try
           clear_status ();
           let text = Js.to_string text_input##.value in
           let layout_name = Js.to_string layout_select##.value in
           let font_size_text = String.trim (Js.to_string font_size_input##.value) in
           let show_borders = Js.to_bool border_checkbox##.checked in
           let include_checkbox = Js.to_bool checkbox_feature_checkbox##.checked in
           let justification = justification_of_string (Js.to_string justification_select##.value) in
           let font = font_of_key (Js.to_string font_select##.value) in

           match float_of_string_opt font_size_text with
           | None -> show_error ("\"" ^ font_size_text ^ "\" is not a valid font size. Enter a number between 6 and 72.")
           | Some font_size when font_size < 6.0 || font_size > 72.0 -> show_error "Font size must be between 6 and 72 pt."
           | Some font_size -> (
               log_message
                 (Printf.sprintf "Generating %s in %s at %gpt, borders=%b checkbox=%b alignment=%s" layout_name font.file font_size show_borders
                    include_checkbox
                    (Js.to_string justification_select##.value));

               match load_font_from_fs font with
               | None -> show_error "Could not load the label font. Try reloading the page; if it keeps happening the page may not have downloaded fully."
               | Some font_bytes -> (
                   let generated =
                     try create_pdf_as_string (create_pdf_with_labels font_bytes text layout_name font_size ~show_borders ~include_checkbox ~justification ())
                     with e -> Error (Printexc.to_string e)
                   in
                   match generated with
                   | Error msg -> show_error ("Could not generate the PDF: " ^ msg)
                   | Ok pdf_content -> (
                       let filename =
                         "labels_" ^ layout_name ^ "_" ^ font.key
                         ^ (if show_borders then "_bordered" else "")
                         ^ (if include_checkbox then "_checkbox" else "")
                         ^ ".pdf"
                       in
                       try
                         download_pdf pdf_content filename;
                         show_info "Label sheet generated. If it did not open, check whether your browser blocked the pop-up."
                       with e -> show_error ("The PDF was generated but could not be opened: " ^ Printexc.to_string e))))
         with e -> show_error ("Something went wrong: " ^ Printexc.to_string e));
        Js._true);

  Dom.appendChild form_div button_container;

  Dom.appendChild main_container form_div;
  Dom.appendChild body main_container
