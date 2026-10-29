// logicDwg.dcl - Logic DWG dialog.
// Every "key" here must match a button line in LogicDWG:Buttons (logicDwg.lsp).
// Layout:   left column  : Yard Tool, Settings
//           right column : Map Zone, Gradient Tool, Gradient Index Plan, QC
// All buttons use the same size (see "btn"). Rows in the Yard Tool box have a
// 14-wide spacer where there is no Export button, so every Generate button
// sits in the same column.

btn : button { width = 14; height = 1.6; fixed_width = true; fixed_height = true; }
cap : text   { width = 22; fixed_width = true; alignment = centered; }
gap : spacer { width = 14; fixed_width = true; }

logicDwg : dialog {
  label = "Logic DWG";

  : row {

    // ---------------- left column ----------------
    : column {
      alignment = top;

      : boxed_column {
        label = "Yard Tool";
        width = 56;
        : row { : cap { label = "Survey Points"; }   : gap { }                                          : btn { key = "sp";          label = "Generate"; } }
        : row { : cap { label = "Rail Track"; }      : btn { key = "trackEx"; label = "Export"; }       : btn { key = "track";       label = "Generate"; } }
        : row { : cap { label = "Chainage"; }        : gap { }                                          : btn { key = "chain";       label = "Generate"; } }
        : row { : cap { label = "Ohe"; }             : gap { }                                          : btn { key = "ohe";         label = "Generate"; } }
        : row { : cap { label = "Building"; }        : gap { }                                          : btn { key = "building";    label = "Generate"; } }
        : row { : cap { label = "Boundary"; }        : gap { }                                          : btn { key = "boundary";    label = "Generate"; } }
        : row { : cap { label = "Bank Cutting"; }    : gap { }                                          : btn { key = "bankCutting"; label = "Generate"; } }
        : row { : cap { label = "TR"; }              : gap { }                                          : btn { key = "tr";          label = "Generate"; } }
      }

      : boxed_column {
        label = "Settings";
        width = 56;
        : row { : cap { label = "Dim Settings"; }      : btn { key = "settings"; label = "Apply"; } }
        : row { : cap { label = "Convert Dal Unit"; }  : btn { key = "dimMToFt"; label = "Convert"; } }
        : row { : cap { label = "Rotate Text"; }       : btn { key = "rotText";  label = "Rotate"; } }
      }
    }

    // ---------------- right column ----------------
    : column {
      alignment = top;

      : boxed_row {
        label = "Map Zone";
        width = 46;
        : btn { key = "z43";  label = "43"; }
        : btn { key = "z44";  label = "44"; }
        : btn { key = "zoff"; label = "Off"; }
      }

      : boxed_column {
        label = "Gradient Tool";
        width = 46;
        : row { : cap { label = "Gradient Block"; }  : btn { key = "grad";      label = "Generate"; } }
        : row { : cap { label = "Align Block"; }     : btn { key = "gradAlign"; label = "Align"; } }
        : row { : cap { label = "Clean up Block"; }  : btn { key = "gradClean"; label = "Clean Up"; } }
      }

      : boxed_column {
        label = "Gradient Index Plan";
        width = 46;
        : row { : cap { label = "Index Gradient"; }  : btn { key = "indexGrad";  label = "Generate"; } }
        : row { : cap { label = "Index Curve"; }     : btn { key = "indexCurve"; label = "Generate"; } }
      }

      : boxed_column {
        label = "QC";
        width = 46;
        : row { : cap { label = "Delete Cad Layer"; }  : btn { key = "cadMerge";    label = "Delete"; } }
        : row { : cap { label = "Purge and Audit"; }   : btn { key = "cadOptimize"; label = "Optimize"; } }
        : row { : cap { label = "Block Layer"; }       : btn { key = "blockLayer";  label = "Change"; } }
      }
    }
  }

  spacer_1;
  : row {
    alignment = centered;
    fixed_width = true;
    : btn { key = "cancel"; label = "Cancel"; is_cancel = true; }
  }
}
