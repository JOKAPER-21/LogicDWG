// ============================================================================
// LogicDWG.dcl
// Release: 1.1.2 | Civil 3D 2026
// Version: 02
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
