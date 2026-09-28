// ============================================================================
// logicDwg.dcl  -  Logic DWG launcher dialog
//
// Used by logicDwg.lsp (command LOGICDWG / VIDLOGICDWG).
// Keep this file in the same folder as logicDwg.lsp.
//
// Keys returned to logicDwg.lsp:
//   z43 / z44 / zoff   Zone buttons
//   sp                 Generate Points
//   track              Generate Track
//   close              Close
// ============================================================================

logicDwg : dialog {
  label = "Logic DWG";

  : boxed_row {
    label = "Zone";
    : button { key = "z43";  label = "43";  width = 10; fixed_width = true; }
    : button { key = "z44";  label = "44";  width = 10; fixed_width = true; }
    : button { key = "zoff"; label = "Off"; width = 10; fixed_width = true; }
  }

  : boxed_row {
    label = "DGPS to Survey Point";
    : button { key = "sp"; label = "Generate Points"; width = 34; fixed_width = true; }
  }

  : boxed_row {
    label = "Rail Tracks";
    : button { key = "track"; label = "Generate Track"; width = 34; fixed_width = true; }
  }

  spacer_1;

  : row {
    alignment = centered;
    : button { key = "close"; label = "Close"; width = 12; fixed_width = true; is_cancel = true; }
  }
}
