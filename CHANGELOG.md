# Changelog

### Removal of the ML register allocation hint pass

Removed `OPT-PASS-ML-REGALLOC` and the `:ml-regalloc` pipeline option. The pass
computed register pressure values but never attached them to instructions or
returned them to the register allocator, and no other optimizer feature used
the pass.
