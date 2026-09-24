# Label Maker

A web-based label maker application built with OCaml that generates PDF labels for various Avery formats.

## Demo

Try it online: [https://mtelvers.github.io/label-maker-js/](https://mtelvers.github.io/label-maker-js/)

## Features

- Support for multiple Avery label formats (L7160, L7162, etc.)
- PDF generation with precise positioning
- Choice of a joined cursive font or an easy-read comic font
- Optional reduced page size so printers do not rescale the sheet
- Custom text input with font rendering
- Compiles to JavaScript for web deployment

## Build Instructions

### Prerequisites

- OCaml (>= 4.14)
- Opam package manager
- Dune build system

### Setup

```bash
# Install dependencies
opam install --deps-only .

# Build the project
dune build

# Generate JavaScript for web deployment
dune build bin/label_maker.bc.js
```

### Development

```bash
# Clean build artifacts
dune clean

# Run tests
dune exec bin/test_labels.exe

# Format code
dune fmt
```

## Fonts

Two fonts are embedded in the generated PDF, selectable from the **Font** menu:

| Menu entry | File | Notes |
| --- | --- | --- |
| Cursive (XCCW Joined) | `XCCW_Joined_23a.ttf` | Default. Joined handwriting style for primary classes. |
| Comic Sans style (Comic Relief) | `ComicRelief-Regular.ttf` | Easier for early-years readers who cannot yet read cursive. |

Comic Relief is used in place of Comic Sans MS, which is proprietary and cannot be
redistributed from this repository. It is licensed under the SIL Open Font License 1.1 —
see [`ComicRelief-OFL.txt`](ComicRelief-OFL.txt).

Both fonts are baked into the js_of_ocaml virtual filesystem by the `--file` flags in
`bin/dune`, so the page needs no network access to generate a PDF.

## Supported Label Formats

- **Avery L7160**: 63.5×38.1mm labels, 3×7 grid
- **Avery L7162**: 99.1×33.9mm labels, 2×8 grid

Either layout can be emitted on a reduced page — see **Shrink page to fit printer** below.

## Shrink page to fit printer

Printer drivers set to "fit to printable area" shrink an A4 page so it clears the
unprintable margin, which moves every label. Ticking **Shrink page to fit printer** crops
the blank margin off the page itself, so the driver has nothing oversized to scale. The
labels keep their exact A4 positions; only the page box around them changes.

The page is cropped to what is actually **drawn**, not to the label grid. That matters:
the grid runs closer to the paper edge than the ink does, so on a printer whose
unprintable margin exceeds the grid margin, a grid-cropped page would still be shrunk.
Turning borders off therefore gives a smaller page than leaving them on.

Typical page sizes at 12pt with a checkbox:

| Layout | Normal page | Shrunk, borders off | Shrunk, borders on |
| --- | --- | --- | --- |
| Avery L7160 | 210×297mm (A4) | 191.8×267.0mm | 196.4×267.4mm |
| Avery L7162 | 210×297mm (A4) | 195.2×267.0mm | 198.8×275.8mm |

The crop is deliberately *concentric* with A4 — the same inset is taken off both sides of
each axis, rather than hugging the ink — so the page centre stays on the A4 centre and a
driver that centres the smaller sheet puts every label back exactly where it belongs.
Cropping tight to the ink would instead offset the sheet by half the difference between
opposite margins. The crop is also capped at 15mm per edge, so centred or right-aligned
short text cannot produce a page a fraction of A4 that a driver might scale *up*.

Text sits 8pt (2.8mm) inside each label rather than a tight 3pt. That keeps the ink
9.8mm from the paper edge — well clear of the unprintable margin of the printers in use,
measured at ~8.4mm on one — so the cropped page needs no scaling and there is room for a
printer's own registration offset on top. It also gives the outer columns a visible
margin rather than text that looks pushed up against the die cut.

Leave it off unless labels print out of position. It removes the driver's *reason* to
scale but cannot prevent it: a printer whose unprintable margin exceeds the crop will
still shrink the page. With borders enabled the leftmost column and top row sit close to
the page edge, so part of that 1pt stroke may be clipped.

This option replaces the old "Avery L7160-93" layout entry, which did the same job for one
layout only.

## Architecture

- **lib/**: Core OCaml library for PDF generation and label layouts
- **bin/**: Executables (native and JavaScript compilation)
- **docs/**: Web deployment files with generated JavaScript

## License

MIT License - see the project repository for details.

## Repository

Source code: [https://github.com/mtelvers/label-maker-js](https://github.com/mtelvers/label-maker-js)