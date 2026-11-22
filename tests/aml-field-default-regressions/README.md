# Hosted Field default-wrapper regressions

Seven separately selected programs exercise runtime and module OperationRegion declarations, Field metadata and descriptors, Fatal results, selector lifetime and staged fields. Both strict release and assertion-enabled profiles passed 5,232 checks on this production composition. Three historical feature-negative expectations were corrected as implementation support advanced; these fixtures preserve the remaining assertions.

Run `python3 tests/aml-field-default-regressions/run.py --mode release` or `--mode checked` in the pinned Nix environment with the coordinated TMPDIR. This suite does not invoke a hardware provider and does not establish native deployment or full interpreter proof.
