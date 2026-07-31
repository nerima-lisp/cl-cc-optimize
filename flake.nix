{
  description = "Optimizer subsystem: CFG, SSA, E-graph, peephole, pipeline";

  inputs = {
    # nixos-unstable, not nixpkgs-unstable: it advances only after the NixOS
    # release tests pass, so it is less likely to land a broken build.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Pinned to a release tag. A bare `github:nerima-lisp/cl-weave` follows
    # that repository's default branch, so an upstream push to main would
    # break this repository's CI without warning. This used to be a
    # `flake = false` source tree handed to the runner through an environment
    # variable; it is a normal flake input now, which is what lets ASDF find
    # cl-weave through CL_SOURCE_REGISTRY like any other dependency.
    cl-cc-ast = {
      url = "github:nerima-lisp/cl-cc-ast/v0.2.0";
      flake = false;
    };
    cl-cc-type = {
      url = "github:nerima-lisp/cl-cc-type/v0.1.0";
      flake = false;
    };
    # cl-cc-bootstrap has cut no release tags yet, so a commit SHA is the only
    # immutable reference available; check-conformance.sh accepts a 40-char
    # SHA as equivalent to a tag pin for exactly this reason.
    cl-cc-bootstrap = {
      url = "github:nerima-lisp/cl-cc-bootstrap/b1ff1defbba014ba7b312c882ee8a5e7cabb5cc3";
      flake = false;
    };
    cl-cc-runtime = {
      url = "github:nerima-lisp/cl-cc-runtime/v0.1.0";
      flake = false;
    };
    cl-cc-vm = {
      url = "github:nerima-lisp/cl-cc-vm/d88159a190283aaa39e50ec8e4b3fa2392dad0bd";
      flake = false;
    };
    cl-prolog = {
      url = "github:nerima-lisp/cl-prolog/v1.1.0";
      flake = false;
    };
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.0.2";
      flake = false;
    };
    # cl-log-kit 2.0.0 drops its zero-runtime-dependency guarantee in favor of
    # these three nerima-lisp packages, used directly with no adapter layer:
    # calendar/zone handling, locks/condition-variables/atomics, and
    # filesystem operations. They are transitive here (cl-cc-runtime pulls in
    # cl-log-kit) but ASDF resolves by name against CL_SOURCE_REGISTRY, so
    # they must be declared as siblings even though nothing in this repository
    # names them directly.
    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v0.2.0";
      flake = false;
    };
    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.2.0";
      flake = false;
    };
    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.2.1";
      flake = false;
    };
    cl-log-kit = {
      url = "github:nerima-lisp/cl-log-kit/v2.0.0";
      flake = false;
    };
    cl-process-kit = {
      url = "github:nerima-lisp/cl-process-kit/v2.0.0";
      flake = false;
    };
    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.0.1";
      flake = false;
    };
    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v2.0.0";
      flake = false;
    };
    # Used only as a source tree (`${cl-weave}//` in sourceRegistry below),
    # never through its flake outputs, so `flake = false` keeps this repo's
    # lock file from also dragging in cl-weave's own input graph (cl-nix-forge,
    # treefmt-nix, and their transitive nixpkgs).
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.1.0";
      flake = false;
    };

    # `flake = true`: checks.paredit-lint below calls its `lib.<system>.mkLintCheck`,
    # a flake output, not just an ASDF source tree.
    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      cl-weave,
      cl-cc-ast,
      cl-cc-type,
      cl-cc-bootstrap,
      cl-cc-runtime,
      cl-cc-vm,
      cl-prolog,
      cl-parser-kit,
      cl-date-kit,
      cl-concurrent-kit,
      cl-host-kit,
      cl-log-kit,
      cl-process-kit,
      cl-json-kit,
      cl-boundary-kit,
      paredit-cli,
      treefmt-nix,
      ...
    }:
    let
      # Only platforms that something actually verifies are declared.
      # x86_64-linux is what CI runs; aarch64-darwin is the development
      # machine, so it is verified on every local `nix flake check`. Dropping
      # aarch64-darwin would make that local run skip every derivation and
      # still report success. aarch64-linux and x86_64-darwin are declared by
      # nobody's verification, so they are not declared here either.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # CL_SOURCE_REGISTRY for the test, coverage and dev environments.
      sourceRegistry = "${cl-weave}//:${cl-cc-ast}//:${cl-cc-type}//:${cl-cc-bootstrap}//:${cl-cc-runtime}//:${cl-cc-vm}//:${cl-prolog}//:${cl-parser-kit}//:${cl-date-kit}//:${cl-concurrent-kit}//:${cl-host-kit}//:${cl-log-kit}//:${cl-process-kit}//:${cl-json-kit}//:${cl-boundary-kit}//:${self}//";

      # Single source of truth for the package version: the `:version` form in
      # cl-cc-optimize.asd. A release only ever edits the .asd file and every Nix
      # package (default + docs) follows automatically. Nix regexes are
      # whole-string anchored and `.` never spans newlines, so the version is
      # extracted line-by-line rather than with one multi-line match.
      version =
        let
          lines = nixpkgs.lib.splitString "\n" (builtins.readFile ./cl-cc-optimize.asd);
          versionLine = builtins.head (
            builtins.filter (line: builtins.match "[[:space:]]*:version \"[^\"]*\"" line != null) lines
          );
        in
        builtins.head (builtins.match "[[:space:]]*:version \"([^\"]*)\"" versionLine);

      # treefmt drives `nix fmt` and the `checks.<system>.formatting` gate.
      # Scope is Nix only: nixfmt (RFC-style) is a zero-footgun, low-diff
      # formatter, whereas YAML formatters mangle the GitHub Actions `on:`
      # key and Markdown reformatting would churn the whole docs tree.
      treefmtEval = forAllSystems (
        system:
        treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
        }
      );
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        rec {
          cl-cc-optimize = pkgs.sbcl.buildASDFSystem {
            pname = "cl-cc-optimize";
            inherit version;
            src = self;
            systems = [ "cl-cc-optimize" ];
          };
          default = cl-cc-optimize;

          # Rendered documentation site (Material for MkDocs).
          # Build fully offline: Material for MkDocs bundles all of its assets,
          # so no network access is required inside the Nix sandbox. --strict
          # promotes broken links and unlisted pages to build failures.
          #
          # Invoked from the repository root with -f docs/mkdocs.yml (not `cd
          # docs`), matching pymdownx.snippets' base_path of "." in
          # docs/mkdocs.yml, which resolves CHANGELOG.md relative to the root.
          docs = pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-cc-optimize-docs";
            inherit version;
            src = pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.unions [
                ./docs/mkdocs.yml
                ./docs/src
                ./CHANGELOG.md
              ];
            };
            nativeBuildInputs = [ pkgs.python3Packages.mkdocs-material ];
            buildPhase = ''
              runHook preBuild
              mkdocs build --strict --config-file docs/mkdocs.yml --site-dir "$out"
              runHook postBuild
            '';
            dontInstall = true;
            meta = {
              description = "Rendered MkDocs (Material) documentation for cl-cc-optimize";
              homepage = "https://github.com/nerima-lisp/cl-cc-optimize";
              license = pkgs.lib.licenses.mit;
            };
          };
        }
      );

      # `nix fmt` entry point.
      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);

      # Granularity lives here, NOT in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute as its own derivation, in parallel,
      # with build caching. Add a check here rather than a job in ci.yml.
      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default =
            pkgs.runCommand "cl-cc-optimize-tests"
              {
                nativeBuildInputs = [
                  pkgs.sbcl
                  pkgs.coreutils
                ];
                CL_SOURCE_REGISTRY = sourceRegistry;
              }
              ''
                export HOME="$TMPDIR/home"
                mkdir -p "$HOME" "$out"
                timeout 600 sbcl --script ${self}/run-tests.lisp
                touch "$out/passed"
              '';

          # Fails `nix flake check` when any tracked file is unformatted,
          # turning the formatter into an enforced CI gate.
          formatting = treefmtEval.${system}.config.build.check self;

          # The docs package builds with `mkdocs --strict`, so a broken link or
          # a page missing from the nav fails the build here rather than
          # surfacing later as a failed docs.yml deploy.
          docs = self.packages.${system}.docs;

          # Fails `nix flake check` when any tracked Lisp source has a
          # structural (unbalanced-parenthesis) parse error -- the same
          # invariant every `paredit inspect check` call in this project's
          # own refactoring workflow already enforces per file, now enforced
          # once over the whole tree as a CI gate.
          paredit-lint = paredit-cli.lib.${system}.mkLintCheck {
            src = self;
            name = "cl-cc-optimize-paredit-lint";
          };
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          test = pkgs.writeShellApplication {
            name = "cl-cc-optimize-test";
            runtimeInputs = [
              pkgs.sbcl
              pkgs.coreutils
            ];
            text = ''
              export CL_SOURCE_REGISTRY="${sourceRegistry}"
              exec timeout 600 sbcl --script ${self}/run-tests.lisp
            '';
          };
        in
        {
          default = {
            type = "app";
            program = "${test}/bin/cl-cc-optimize-test";
          };
          test = {
            type = "app";
            program = "${test}/bin/cl-cc-optimize-test";
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = [ pkgs.sbcl ];
            CL_SOURCE_REGISTRY = sourceRegistry;
          };
        }
      );
    };
}
