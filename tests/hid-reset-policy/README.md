# HID reset policy

Use the pinned Nix development shell, exporting TMPDIR before entry:
`bash tests/hid-reset-policy/run.sh`. Checked and release profiles cover
finite deadline admission, exact1000ms clamp, high-word/sentinel boundaries
and Elan vendor/0xffff classification. No hardware access occurs.

The actual pure helper has separately audited GNATprove levels1+2 evidence:
4 flow obligations (2initialization,2termination) and1 exact deadline
postcondition,7 prover leaves perlevel. Proof uses j2/5s solvers under a
24GiB/no-swap scope. This is not a proof of the device state machine or MMIO.
The guarded packet receipt preserves projects, commands, inputs and .spark
claim-graph audits for reproduction.
