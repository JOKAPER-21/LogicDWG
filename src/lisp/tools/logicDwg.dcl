// logicDwg.dcl - matches LogicDWG:Layout in logicDwg.lsp
logicDwg : dialog {
  label = "Logic DWG";
  : boxed_row {
    label = "Map Zone";
    : button { key = "z43"; label = "43"; width = 14; fixed_width = true; }
    : button { key = "z44"; label = "44"; width = 14; fixed_width = true; }
    : button { key = "zoff"; label = "Off"; width = 14; fixed_width = true; }
  }
  : boxed_column {
    label = "Generate from CSV";
    : row {
      : text { label = "Survey points"; width = 18; fixed_width = true; }
      : button { key = "sp"; label = "Generate"; width = 14; fixed_width = true; }
    }
    : row {
      : text { label = "Rail track"; width = 18; fixed_width = true; }
      : button { key = "track"; label = "Generate"; width = 14; fixed_width = true; }
    }
    : row {
      : text { label = "Chainage Runner"; width = 18; fixed_width = true; }
      : button { key = "chain"; label = "Generate"; width = 14; fixed_width = true; }
    }
    : row {
      : text { label = "Ohe"; width = 18; fixed_width = true; }
      : button { key = "ohe"; label = "Generate"; width = 14; fixed_width = true; }
    }
  }
  : boxed_row {
    label = "CAD Tools";
    : button { key = "cadMerge"; label = "Cad Merge Layer"; width = 14; fixed_width = true; }
    : button { key = "settings"; label = "Settings"; width = 12; fixed_width = true; }
  }
  spacer_1;
  : row {
    alignment = centered;
    : button { key = "close"; label = "Cancel"; width = 12; fixed_width = true; is_cancel = true; }
  }
}
