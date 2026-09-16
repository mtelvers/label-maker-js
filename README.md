# Label Maker

A web-based label maker application built with OCaml that generates PDF labels for various Avery formats.

## Demo

Try it online: [https://mtelvers.github.io/label-maker-js/](https://mtelvers.github.io/label-maker-js/)

## Features

- Support for multiple Avery label formats (L7160, L7162, etc.)
- PDF generation with precise positioning
- Choice of a joined cursive font or an easy-read comic font
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
- **Avery L7160-93**: as L7160, but on a 196×267mm page instead of A4

### About L7160-93

Printer drivers set to "fit to printable area" shrink an A4 page so it clears the
unprintable margin, which moves every label. L7160-93 sidesteps this by cropping the
blank margin off the page itself: the labels keep their exact A4 positions, but the page
box is reduced to 196×267mm (93.3% of A4's width — hence the name) so the driver has
nothing oversized to scale.

The crop is deliberately *concentric* with A4 — the same inset is taken off both sides of
each axis — so the page centre stays on the A4 centre and a driver that centres the
smaller sheet puts every label back exactly where it belongs.

Use it only if labels print misaligned with the plain L7160 layout. It removes the
driver's *reason* to scale but cannot prevent it: a printer whose unprintable margin
exceeds the crop will still shrink the page, and with less slack than before.

## Architecture

- **lib/**: Core OCaml library for PDF generation and label layouts
- **bin/**: Executables (native and JavaScript compilation)
- **docs/**: Web deployment files with generated JavaScript

## License

MIT License - see the project repository for details.

## Repository

Source code: [https://github.com/mtelvers/label-maker-js](https://github.com/mtelvers/label-maker-js)