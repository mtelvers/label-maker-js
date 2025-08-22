# Label Maker

A web-based label maker application built with OCaml that generates PDF labels for various Avery formats.

## Demo

Try it online: [https://mtelvers.github.io/label-maker-js/](https://mtelvers.github.io/label-maker-js/)

## Features

- Support for multiple Avery label formats (L7160, L7162, etc.)
- PDF generation with precise positioning
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
dune exec ocamlformat -- --inplace lib/*.ml bin/*.ml
```

## Supported Label Formats

- **Avery L7160**: 63.5×38.1mm labels, 3×7 grid
- **Avery L7162**: 99.1×33.9mm labels, 2×8 grid

## Architecture

- **lib/**: Core OCaml library for PDF generation and label layouts
- **bin/**: Executables (native and JavaScript compilation)
- **docs/**: Web deployment files with generated JavaScript

## License

MIT License - see the project repository for details.

## Repository

Source code: [https://github.com/mtelvers/label-maker-js](https://github.com/mtelvers/label-maker-js)