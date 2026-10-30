//==========================================================================
// VidPublish.dcl
// Dialog box definition for the VID Publish tool
// AutoCAD Civil 3D 2026
//==========================================================================
vid_publish_dlg : dialog {
    label = "VID Publish";
    // ---------------------------------------------------------
    // Artist
    // ---------------------------------------------------------
    : boxed_column {
        label = "Artist";
        : popup_list {
            key = "artist_list";
            edit_width = 60;
        }
        spacer;
    }
    // ---------------------------------------------------------
    // Yard
    // ---------------------------------------------------------
    : boxed_column {
        label = "Select Yard:";
        : list_box {
            key = "yard_list";
            width = 60;
            height = 30;
            fixed_width = false;
            fixed_height = true;
            multiple_select = false;
        }
        // Server version folder this publish will land in (set from VidPublish.lsp)
        : text {
            key = "target_preview";
            value = "";
            width = 70;
        }
        spacer;
    }
    // ---------------------------------------------------------
    // Command
    // ---------------------------------------------------------
    : boxed_column {
        label = "Command";
        : edit_box {
            key = "cmd_text";
            edit_width = 60;
        }
        spacer;
        : toggle {
            key = "pdf_check";
            label = "Generate PDF";
            value = "1";
        }
        spacer;
    }
    // ---------------------------------------------------------
    // Buttons
    // ---------------------------------------------------------
    : row {
        : button {
            key = "accept";
            label = "Publish";
            is_default = true;
            width = 15;
            height = 2;
        }
        : button {
            key = "cancel";
            label = "Cancel";
            is_cancel = true;
            width = 15;
            height = 2;
        }
    }
}
