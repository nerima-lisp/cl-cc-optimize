# Development

## Build

```console
$ nix build
```

Builds the `cl-cc-optimize` ASDF system as a Nix package via
`pkgs.sbcl.buildASDFSystem`.

## Test

```console
$ nix flake check --print-build-logs
```

Runs every `checks.*` output: the cl-weave test suite (`checks.default`),
Nix formatting (`checks.formatting`), this documentation site
(`checks.docs`), and a structural parse-validity gate over every tracked
Lisp source file (`checks.paredit-lint`, via `nerima-lisp/paredit-cli`'s
`mkLintCheck`). `checks.default` runs under a 600-second `timeout` so a hung
test fails the build instead of hanging CI.

To run only the test suite, without formatting or docs:

```console
$ nix run .#test
```

Both entry points load `run-tests.lisp` at the repository root, which
registers this checkout with ASDF and calls
`(asdf:test-system "cl-cc-optimize")`. That in turn runs
`cl-weave:run-all` with `:pass-with-no-tests nil`, so a suite that registers
zero tests is a failure, not a silent pass.

## Coverage

`cl-weave:run-all` measures coverage through `sb-cover` when called with
`:coverage t`. The `test-op` this repository's `run-tests.lisp` drives does
not pass that flag, so measure it directly from a `nix develop` shell:

```lisp
(asdf:load-system "cl-cc-optimize/test")
(cl-weave:run-all :coverage t
                   :coverage-report-directory "coverage/"
                   :coverage-minimum-expression 90
                   :coverage-minimum-branch 90
                   :pass-with-no-tests nil)
```

The nerima-lisp org floor is 90% expression and branch coverage, without
regressing the previous release (`CODING_STANDARD.md`'s Test section).

## Format

```console
$ nix fmt
```

Formats every tracked Nix file with `nixfmt` via `treefmt`. Lisp source is
not reformatted automatically -- `CODING_STANDARD.md` explains why -- and is
reviewed by hand against the 100-column, file-length, and naming rules in
that document.

## Lint and refactor

This repository is edited with
[`paredit-cli`](https://github.com/nerima-lisp/paredit-cli), a structure-aware
editor for Common Lisp, rather than by hand-balancing parentheses:

```console
$ paredit inspect lint src/
$ paredit refactor plan --help
```

## Local development shell

```console
$ nix develop
```

Provides `sbcl` with `CL_SOURCE_REGISTRY` already pointed at every sibling
package this repository depends on, at the versions pinned in `flake.nix`.
