// logicDwg.dcl - Logic DWG dialog.
// Every "key" here must match a button line in LogicDWG:Buttons (logicDwg.lsp).

btn : button { width = 14; fixed_width = true; }
cap : text   { width = 18; fixed_width = true; }

logicDwg : dialog {
  label = "Logic DWG";

  : boxed_row {
    label = "Map Zone";
    : btn { key = "z43";  label = "43"; }
    : btn { key = "z44";  label = "44"; }
    : btn { key = "zoff"; label = "Off"; }
  }

  : boxed_column {
    label = "Generate from CSV";
    : row { : cap { label = "Survey points"; }   : btn { key = "sp";    label = "Generate"; } }
    : row { : cap { label = "Rail track"; }      : btn { key = "track"; label = "Generate"; } }
    : row { : cap { label = "Chainage Runner"; } : btn { key = "chainRev"; label = "Reverse"; } : btn { key = "chain"; label = "Generate"; } }
    : row { : cap { label = "Ohe"; }             : btn { key = "ohe";   label = "Generate"; } }
  }

  : boxed_row {
    label = "Settings";
    : btn { key = "cadMerge"; label = "Cad Merge Layer"; }
    : btn { key = "settings"; label = "Dim settings"; }
  }

  spacer_1;
  : row {
    alignment = centered;
    fixed_width = true;
    : button { key = "cancel"; label = "Cancel"; width = 12; fixed_width = true; is_cancel = true; }
  }
}
