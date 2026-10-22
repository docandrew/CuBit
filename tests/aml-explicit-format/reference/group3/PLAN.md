# Late source-slot discrimination

Existing 66 observations do not distinguish a saved Local value from rereading that Local after target evaluation. DORDER uses SRC0 returning an immediate integer and independent MARK effects. The current collecting fixture TARG uses SRC0 returning a Buffer; TGT0 mutates its own Local0, not the caller source slot. Thus neither establishes the reused Resolve_Mid_Operand late-slot policy.

Four normal tables test Decimal and Hex, each DSDT revision 1/2. TEST sets Local0 to0x11. During target Index evaluation, TGT0 receives RefOf(Local0), stores0x22 through its argument, sets MARK34 and returns index0. Formatting result is observed as full ToBuffer bytes. Saved-source semantics predicts decimal17 or hex11; late-source semantics predicts decimal34 or hex22. Exact hex spelling remains observed, not guessed. MARK34 witnesses target execution, while independent WIDT verifies width. This is an evidence discriminator, not an expected pass assertion.

Reuse frozen oracle harness with normal iasl -oa, method-specific opcode disassembly,5second process timeout,128MiB address-space and64MiB stack. No forced AML. Compile rejection or runtime failure remains a separate outcome. Source prepared only; no worker launched pending root reassignment. No CuBit production/test edits.
