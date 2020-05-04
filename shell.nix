{ ghc ? "default"
}:
# Load an interactive environment:
(import ./. {
  inherit ghc;
}).interactive
