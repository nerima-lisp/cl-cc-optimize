# Installation

`cl-cc-optimize` is distributed as source only, through the nerima-lisp Nix
flake ecosystem. There is no Quicklisp distribution.

## Add the flake input

```nix
cl-cc-optimize = {
  url = "github:nerima-lisp/cl-cc-optimize/v0.1.0";
  flake = false;
};
```

Append `${cl-cc-optimize}//` to your `CL_SOURCE_REGISTRY` so ASDF can find it,
the same way `cl-cc-optimize`'s own `flake.nix` lists its siblings.

## Add the ASDF dependency

```lisp
(asdf:defsystem "your-system"
  :depends-on ("cl-cc-optimize")
  ...)
```

## Load and verify

```lisp
(asdf:load-system "cl-cc-optimize")
;; => T

(cl-cc/optimize:optimize-instructions
 (list (cl-cc/vm:make-vm-const :dst :r0 :value 1)
       (cl-cc/vm:make-vm-halt :reg :r0)))
;; => a non-empty instruction list
```

If `asdf:load-system` signals `missing-dependency`, the sibling naming
`missing-dependency`'s `:requires` slot is absent from your source registry;
add its flake input the same way as `cl-cc-optimize` itself.

See [Development](development.md) for building and testing this repository
from a checkout rather than as a dependency.
