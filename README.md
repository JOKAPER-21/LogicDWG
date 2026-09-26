# LogicDWG

A small AutoCAD Civil 3D LISP toolset for DGPS survey CSV import, packaged
behind one launcher dialog.

## Folder structure

```
LogicDWG/
├── README.md
├── loader.lsp                              <- load this one file to get everything
└── src/
    └── lisp/
        ├── logicdwg/
        │   └── logicdwg_v03.lsp            <- VIDLOGICDWG launcher dialog
        ├── sm/
        │   └── sm.lsp                      <- SM  (Zone / GeoMap: 43 / 44 / Off)
        ├── vdgpstosp/
        │   └── vdgpstosp_v01.lsp           <- VIDDGPSTOSP  (DGPS CSV -> survey points)
        └── dgps2pline/
            └── DGPS2PLINE_v12.LSP          <- DGPS2PLINE (DGPS CSV -> survey points + polyline)
```

## Install / Load

1. Copy the whole `LogicDWG` folder anywhere on disk (a synced git folder,
   e.g. `D:\git\LogicDWG`, works fine - the loader finds its own location).
2. In AutoCAD Civil 3D: `APPLOAD` -> browse to `LogicDWG\loader.lsp` -> Load.
   This loads all four tools in the correct order.
3. Type `VIDLOGICDWG` to open the launcher dialog.

To load automatically every session, add `loader.lsp` to your Startup Suite
in `APPLOAD`, or `(load "D:/git/LogicDWG/loader.lsp")` in your `acaddoc.lsp`.

## The launcher (`VIDLOGICDWG`)

A single dialog titled **Logic DWG** with:

| Section                  | Control(s)                       | Runs          |
|---------------------------|-----------------------------------|---------------|
| Zone                      | `[43]` `[44]` `[Off]`             | MAPCSASSIGN / GEOMAP / ZOOM (same actions as `SM`) |
| DGPS to Survey Point      | `[Generate Points]`               | `VIDDGPSTOSP`   |
| Rail Tracks               | `[Generate Track]`                | `DGPS2PLINE`  |

`VIDDGPSTOSP` and `DGPS2PLINE` are invoked as direct LISP function calls
(`(c:VIDDGPSTOSP)` / `(c:DGPS2PLINE)`) rather than through AutoCAD's command
lookup, which avoids an "Unknown command" quirk that can occur when a
custom command is dispatched by name from inside another already-running
command. The Zone buttons issue the same native `MAPCSASSIGN` / `GEOMAP` /
`ZOOM` commands `SM` itself uses, directly - `sm.lsp` is unmodified and
still works exactly as before when run on its own.

## Commands (can also be run directly, without the dialog)

- `SM` - assign UTM Zone 43N / 44N coordinate system and turn on GeoMap
  (Hybrid, zoom-extents), or turn GeoMap off.
- `VIDDGPSTOSP` - import a DGPS survey CSV and create survey-point graphics
  (POINT + Point Name / Code / Elevation MTEXT) for every valid row.
- `DGPS2PLINE` - same CSV import, plus groups records by Code and connects
  each group into a 2D polyline or 3D polyline (user's choice) in Local
  Time order.

## Versions in this package

- `logicdwg_v03.lsp` - fixes VIDDGPSTOSP/DGPS2PLINE failing to launch from
  the dialog ("Unknown command") by calling their functions directly
  instead of through AutoCAD's command dispatcher; Zone buttons now call
  MAPCSASSIGN/GEOMAP/ZOOM directly instead of re-dispatching to `SM`.
- `vdgpstosp_v01.lsp`, `DGPS2PLINE_v12.LSP`, `sm.lsp` - included as supplied,
  unmodified.
