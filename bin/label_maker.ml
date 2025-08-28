(* simplified js_of_ocaml version using the library *)
open Js_of_ocaml
open Label_maker_lib.Pdf_generator

let log_message _msg = ()

let check_font_data_integrity_js font_bytes expected_size =
  let actual_size = Bytes.length font_bytes in
  let first_4_bytes =
    if actual_size >= 4 then
      Printf.sprintf "%02X %02X %02X %02X"
        (int_of_char (Bytes.get font_bytes 0))
        (int_of_char (Bytes.get font_bytes 1))
        (int_of_char (Bytes.get font_bytes 2))
        (int_of_char (Bytes.get font_bytes 3))
    else "INSUFFICIENT_DATA"
  in
  let last_4_bytes =
    if actual_size >= 4 then
      Printf.sprintf "%02X %02X %02X %02X"
        (int_of_char (Bytes.get font_bytes (actual_size - 4)))
        (int_of_char (Bytes.get font_bytes (actual_size - 3)))
        (int_of_char (Bytes.get font_bytes (actual_size - 2)))
        (int_of_char (Bytes.get font_bytes (actual_size - 1)))
    else "INSUFFICIENT_DATA"
  in

  log_message ("FONT_DEBUG: Expected size: " ^ string_of_int expected_size ^ ", Actual size: " ^ string_of_int actual_size);
  log_message ("FONT_DEBUG: First 4 bytes: " ^ first_4_bytes);
  log_message ("FONT_DEBUG: Last 4 bytes: " ^ last_4_bytes);

  (* Check for TTF/OTF magic numbers *)
  let is_valid_font =
    if actual_size >= 4 then
      let magic = Printf.sprintf "%c%c%c%c" (Bytes.get font_bytes 0) (Bytes.get font_bytes 1) (Bytes.get font_bytes 2) (Bytes.get font_bytes 3) in
      magic = "\x00\x01\x00\x00" || magic = "OTTO" || magic = "true" || magic = "ttcf"
    else false
  in

  log_message ("FONT_DEBUG: Valid font magic number: " ^ string_of_bool is_valid_font);

  (* Calculate simple checksum *)
  let checksum = ref 0 in
  for i = 0 to min 99 (actual_size - 1) do
    (* First 100 bytes checksum *)
    checksum := !checksum + int_of_char (Bytes.get font_bytes i)
  done;
  log_message ("FONT_DEBUG: First 100 bytes checksum: " ^ string_of_int !checksum);

  (actual_size = expected_size, is_valid_font, !checksum)

let load_font_from_fs filename =
  try
    log_message ("Loading font from js_of_ocaml virtual filesystem: " ^ filename);
    let ic = open_in_bin filename in
    let length = in_channel_length ic in
    let bytes = Bytes.create length in
    really_input ic bytes 0 length;
    close_in ic;
    log_message ("Font loaded successfully from VFS: " ^ string_of_int length ^ " bytes");

    (* Check font data integrity *)
    let size_ok, valid_magic, checksum = check_font_data_integrity_js bytes 63200 in
    log_message
      ("Font integrity check: size_ok=" ^ string_of_bool size_ok ^ ", valid_magic=" ^ string_of_bool valid_magic ^ ", checksum=" ^ string_of_int checksum);

    Some bytes
  with e ->
    log_message ("Font loading error: " ^ Printexc.to_string e);
    None

let download_pdf_binary_safe pdf_content filename =
  log_message ("Creating binary-safe PDF download for " ^ filename);
  log_message ("PDF content length: " ^ string_of_int (String.length pdf_content) ^ " bytes");

  (* Convert string to Uint8Array for proper binary handling *)
  let length = String.length pdf_content in
  let uint8_array = new%js Typed_array.uint8Array length in

  for i = 0 to length - 1 do
    Typed_array.set uint8_array i (int_of_char (String.get pdf_content i))
  done;

  log_message "Converted to Uint8Array successfully";

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
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "URL.revokeObjectURL") [| url |]);

  log_message "Binary-safe PDF download completed"

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

  generate_button##.onclick :=
    Dom_html.handler (fun _ ->
        (try
           let text = Js.to_string text_input##.value in
           let layout_name = Js.to_string layout_select##.value in
           let font_size = float_of_string (Js.to_string font_size_input##.value) in
           let show_borders = Js.to_bool border_checkbox##.checked in
           let include_checkbox = Js.to_bool checkbox_feature_checkbox##.checked in

           log_message
             ("Generating labels with text: '" ^ text ^ "', layout: " ^ layout_name ^ ", font size: " ^ string_of_float font_size ^ ", borders: "
            ^ string_of_bool show_borders ^ ", checkbox: " ^ string_of_bool include_checkbox);

           match load_font_from_fs "XCCW_Joined_23a.ttf" with
           | Some font_bytes -> (
               log_message ("Font loaded successfully! Size: " ^ string_of_int (Bytes.length font_bytes) ^ " bytes");
               try
                 log_message "Creating multi-label PDF...";
                 let pdf = create_pdf_with_labels font_bytes text layout_name font_size ~show_borders ~include_checkbox () in
                 let pdf_content = create_pdf_as_string pdf in
                 if String.length pdf_content > 6 && String.sub pdf_content 0 6 = "Error:" then log_message pdf_content
                 else
                   let filename =
                     "labels_" ^ layout_name ^ (if show_borders then "_bordered" else "") ^ (if include_checkbox then "_checkbox" else "") ^ ".pdf"
                   in
                   let () = download_pdf pdf_content filename in
                   log_message ("Label PDF generated and opened in browser: " ^ filename)
               with e -> log_message ("Error creating PDF: " ^ Printexc.to_string e))
           | None -> log_message "Failed to load custom font. Make sure XCCW_Joined_23a.ttf is uploaded to the virtual file system."
         with e -> log_message ("Error: " ^ Printexc.to_string e));
        Js._true);

  Dom.appendChild form_div button_container;

  Dom.appendChild main_container form_div;
  Dom.appendChild body main_container
