# LogicDWG

AutoLISP toolkit for **AutoCAD Civil 3D 2026** that turns DGPS survey CSV files into drawing geometry, sets the map zone, and adds running chainage along a track. Everything is opened from one dialog (`VIDLOGICDWG`) or run directly as a command.

Release: 1.1.3

---

## Contents

| Command | File | What it does |
|---|---|---|
| `VIDLOGICDWG` (`LOGICDWG`) | `tools/logicDwg.lsp` | Dialog with Map Zone buttons and the CSV tools |
| `VIDMAPZONE` | `tools/vidMapZone.lsp` | Command-line map zone: Z43 / Z44 / Off |
| `VIDDGPSTOSP` | `tools/vidDgpsToSp.lsp` | DGPS CSV to survey points with labels |
| `VIDDGPSTOLINE` | `tools/vidDgpsToLine.lsp` | DGPS CSV to polylines or 3D polylines, one layer per code |
| `VIDDGPSTOOHE` | `tools/vidDgpsToOhe.lsp` | DGPS CSV to OHE rectangles and stacked text, aligned to the track |
| `VIDCHAINAGERUNNER` | `tools/vidChainageRunner.lsp` | Running chainage marks along a selected polyline |

## Folder layout

```
Support\vid\
  vidLoader.lsp          <- load this one
  tools\
    logicDwg.lsp
    logicDwg.dcl         (optional, see Notes)
    vidChainageRunner.lsp
    vidDgpsToLine.lsp
    vidDgpsToOhe.lsp
    vidDgpsToSp.lsp
    vidMapZone.lsp
```

`vidLoader.lsp` must sit directly above the `tools` folder.

## Installation

1. Copy the `vid` folder to:
   `C:\Users\<you>\AppData\Roaming\Autodesk\C3D 2026\enu\Support\`
2. In Civil 3D, load the loader:
   ```
   (load "C:/Users/<you>/AppData/Roaming/Autodesk/C3D 2026/enu/Support/vid/vidLoader.lsp")
   ```
3. The console lists each file as `[OK]`, `[MISSING]` or `[ERROR]`, then prints the available commands.

### Load automatically at startup

Use one of these:

- **APPLOAD > Startup Suite > Contents > Add** and select `vidLoader.lsp`, or
- add the `vid` folder to **OPTIONS > Files > Support File Search Path** and add
  `(load "vidLoader.lsp")` to `acaddoc.lsp`.

## Usage

### Map zone
`VIDLOGICDWG`, then click **43**, **44** or **Off**, or type `VIDMAPZONE`.
Assigns `UTM84-43N` or `UTM84-44N`, turns on GeoMap (Hybrid) and zooms to extents. **Off** turns GeoMap off.

### DGPS CSV tools
All three read a DGPS export CSV and detect columns by header name (a UTF-8 BOM is handled).

| Tool | Required headers | Optional |
|---|---|---|
| `VIDDGPSTOSP`, `VIDDGPSTOLINE` | `Point Name`, `Code`, `Northing`, `Easting`, `Elevation` | `Local Time` |
| `VIDDGPSTOOHE` | `Code`, `Northing`, `Easting` | Header spelling is flexible (case, spaces, `_`, `-` ignored) |

**VIDDGPSTOSP** places a point plus MTEXT labels (point name, code, elevation) at each row. A dialog asks for the layer mode:
- **Custom**: everything on one layer (default `1-SURVEY`)
- **CSV Code**: one `sp_<Code>` layer per code

**VIDDGPSTOLINE** asks for `Polyline` or `3Dpolyline` and draws one line per code on layer `pl_<Code>` or `3dpl_<Code>`.
- Points are ordered by `Local Time`, then nudged into forward order using the nearest point within a look-ahead window (default 8).
- Change the window with `(setq DGPS-ForwardWindow 12)`.
- Codes with a single point are skipped.

**VIDDGPSTOOHE** reads rows whose code starts with `OH`, `OM`, `OHE` or `PORTAL`. For each one it finds the nearest polyline on layer `1-track`, draws a 0.3 m square rotated to the track direction, and adds text (height 1.5). Output goes to layer `1-OHE-CSV`. Choose `Auto` (all track polylines) or `Select`.

### Chainage runner
`VIDCHAINAGERUNNER`: select the polyline in the direction of increasing chainage, choose Forward or Backward, then enter start KM, start meter and the increment (default 100). Marks are drawn on layer `1-CHAINAGE` at a 1.5 offset from the line.

## Troubleshooting

| Message | Cause and fix |
|---|---|
| `could not find the LogicDWG folder` | `tools\logicDwg.lsp` is not next to the loader. Check the folder layout, or add `vid` to the Support File Search Path. |
| `[MISSING] <file>` | The file is not in `vid\tools`. |
| `[ERROR] <file> - malformed list on input` | Smart quotes or non-ASCII characters got into the file. Re-save as plain ASCII or UTF-8 without pasted formatting. |
| `Command ... is not loaded` in the dialog | Run `vidLoader.lsp` first. |
| `Required headers missing` | The CSV header row does not contain the required column names above. |
| `No track polylines found on layer 1-track` | Put the track polylines on layer `1-track`, or use `Select` mode. |
| `Unknown command "MAPCSASSIGN"` | Map and GeoMap commands need the Civil 3D / Map 3D environment. |

## Notes for maintainers

- **Editing the dialog:** edit `LogicDWG:Layout` at the top of `logicDwg.lsp`. By default the dialog is generated into `%TEMP%` at runtime, so no DCL file is needed. `logicDwg.dcl` is only used if `LogicDWG:UseDclFile` is set to `T`.
- **Adding a tool:** add its file to the `files` list in `vidLoader.lsp`, then add a `CMD` line in `LogicDWG:Layout`.
- **Zone codes:** edit `LogicDWG:ZoneCS` in `logicDwg.lsp`.
- **Encoding:** keep all `.lsp` files plain ASCII. Avoid emoji and smart quotes.