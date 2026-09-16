(* Simplified font extraction using only basic Otfm functions.

   Nothing here writes to stdout: in the js_of_ocaml build that would be
   console output on every PDF generation. Unreadable tables fall back to
   sensible defaults instead. *)
let extract_basic_font_data font_bytes =
  try
    let bytes_string = Bytes.to_string font_bytes in
    let d = Otfm.decoder (`String bytes_string) in

    (* Extract PostScript name *)
    let ps_name = match Otfm.postscript_name d with Ok (Some name) -> name | Ok None | Error _ -> "XCCWJoined23a" in

    (* Extract basic metrics from head table *)
    let bbox, units_per_em =
      match Otfm.head d with
      | Ok head ->
          let upm = float_of_int head.Otfm.head_units_per_em in
          let scale = 1000.0 /. upm in
          let left = float_of_int head.Otfm.head_xmin *. scale in
          let bottom = float_of_int head.Otfm.head_ymin *. scale in
          let right = float_of_int head.Otfm.head_xmax *. scale in
          let top = float_of_int head.Otfm.head_ymax *. scale in
          ((left, bottom, right, top), upm)
      | Error _ -> ((-200.0, -200.0, 1200.0, 1000.0), 1000.0)
    in

    (* Extract vertical metrics *)
    let ascent, descent =
      match Otfm.hhea d with
      | Ok hhea ->
          let scale = 1000.0 /. units_per_em in
          let asc = float_of_int hhea.Otfm.hhea_ascender *. scale in
          let desc = float_of_int hhea.Otfm.hhea_descender *. scale in
          (asc, desc)
      | Error _ -> (800.0, -200.0)
    in

    (ps_name, bbox, ascent, descent, units_per_em)
  with _ -> ("XCCWJoined23a", (-200.0, -200.0, 1200.0, 1000.0), 800.0, -200.0, 1000.0)

(* Extract proper character mappings and widths from font *)
let extract_cmap_and_widths font_bytes =
  try
    let bytes_string = Bytes.to_string font_bytes in
    let d = Otfm.decoder (`String bytes_string) in

    (* Get character mappings from cmap table *)
    let char_to_glyph = ref [] in
    (try
       let cmap_fold = Otfm.cmap d in
       let _ =
         cmap_fold
           (fun acc _map_kind cp_range glyph_id ->
             let start_cp, end_cp = cp_range in
             for cp = start_cp to min end_cp 255 do
               if cp >= 32 && cp <= 126 then (* ASCII printable range *)
                 char_to_glyph := (cp, glyph_id + (cp - start_cp)) :: !char_to_glyph
             done;
             acc)
           ()
       in
       ()
     with _ ->
       (* Fallback: create basic ASCII mapping *)
       for i = 32 to 126 do
         char_to_glyph := (i, i - 29) :: !char_to_glyph
       done);

    (* Get glyph widths from hmtx table *)
    let glyph_widths = ref [] in
    (try
       let hmtx_fold = Otfm.hmtx d in
       let _ =
         hmtx_fold
           (fun acc glyph_id advance_width _lsb ->
             glyph_widths := (glyph_id, advance_width) :: !glyph_widths;
             acc)
           ()
       in
       ()
     with _ -> (* fall through to the default widths below *) ());

    (* Get scaling factor *)
    let scale_factor = match Otfm.head d with Ok head -> 1000.0 /. float_of_int head.Otfm.head_units_per_em | Error _ -> 1.0 in

    (* Create width array with proper character widths *)
    let widths = Array.make 256 600.0 in
    (* default width *)
    List.iter
      (fun (char_code, glyph_id) ->
        match List.assoc_opt glyph_id !glyph_widths with
        | Some raw_width ->
            let scaled_width = float_of_int raw_width *. scale_factor in
            if char_code >= 0 && char_code < 256 then widths.(char_code) <- scaled_width
        | None -> (* keep the default width for this character *) ())
      !char_to_glyph;

    (!char_to_glyph, widths, scale_factor)
  with _ ->
    (* Emergency fallback *)
    let widths = Array.make 256 600.0 in
    ([], widths, 1.0)

(* Create proper ToUnicode CMap with actual character mappings *)
let create_proper_cmap char_mappings subset_name =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "/CIDInit /ProcSet findresource begin\n";
  Buffer.add_string buffer "12 dict begin\n";
  Buffer.add_string buffer "begincmap\n";
  Buffer.add_string buffer "/CIDSystemInfo\n";
  Buffer.add_string buffer ("<< /Registry (" ^ subset_name ^ ")\n");
  Buffer.add_string buffer ("   /Ordering (" ^ subset_name ^ ")\n");
  Buffer.add_string buffer "   /Supplement 0\n";
  Buffer.add_string buffer ">> def\n";
  Buffer.add_string buffer ("/CMapName /" ^ subset_name ^ " def\n");
  Buffer.add_string buffer "/CMapType 2 def\n";
  Buffer.add_string buffer "1 begincodespacerange\n";
  Buffer.add_string buffer "<00> <FF>\n";
  Buffer.add_string buffer "endcodespacerange\n";

  let mapping_count = List.length char_mappings in
  if mapping_count > 0 then (
    Buffer.add_string buffer (string_of_int mapping_count ^ " beginbfchar\n");
    List.iter (fun (char_code, _) -> Printf.bprintf buffer "<%02X> <%04X>\n" char_code char_code) (List.rev char_mappings);
    Buffer.add_string buffer "endbfchar\n")
  else (
    (* Fallback: create basic ASCII mapping *)
    Buffer.add_string buffer "95 beginbfchar\n";
    for i = 32 to 126 do
      Printf.bprintf buffer "<%02X> <%04X>\n" i i
    done;
    Buffer.add_string buffer "endbfchar\n");

  Buffer.add_string buffer "endcmap\n";
  Buffer.add_string buffer "CMapName currentdict /CMap defineresource pop\n";
  Buffer.add_string buffer "end\n";
  Buffer.add_string buffer "end";
  Buffer.contents buffer
