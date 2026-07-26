# cl-cc-optimize

The optimizer for the [cl-cc](https://github.com/nerima-lisp/cl-cc) Common Lisp
compiler: CFG and SSA construction, dataflow analysis, inlining, devirtualization,
speculation, memory optimization and the pass pipeline.

## Why this could be extracted

cl-cc's split design gated this on §5-2 — hardening `cl-cc/vm`'s public contract.
An external repository cannot reach another package's internal symbols, so every
`cl-cc/vm::` reference here was a place the extraction would break, and there
were 75 of them.

They are gone. `t/` asserts that directly by scanning `src/`, because a single
new one would reintroduce the coupling and would otherwise surface only as a
broken build.

## Usage

```lisp
(asdf:load-system "cl-cc-optimize")
```

## Development

```sh
nix develop
nix flake check
```

## License

MIT
