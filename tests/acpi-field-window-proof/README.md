# Field frame/window contracts

Run bash tests/acpi-field-window-proof/run.sh in the pinned Nix environment, with TMPDIR=/home/doc/cubit-build-tmp set before Nix. The runner pins real external CVC5/Z3 and GNATprove, one worker, five-second solver budgets,180-second proof limits and24GiB address space. It refuses unresolved checks, non-SPARK entities, Assume or skipped analysis.

Configured depth4/16 ordinary instances verify the actual generic Scope implementation. Model configuration ranges over all admission/window flags, finite or invalid deadlines, lock outcomes and post-acquire/release cleanup observations; callbacks return normally and do not reenter or mutate the owning context. The model Acquire precondition requires a finite deadline; its call-site obligation is proved, and a separate proved assertion checks that the lock request is bounded by the captured owner window. No public Scope precondition is added.

Four normal-return contracts cover exactly-one accepted frame entry, no frame consumption on refusal, fixed captured deadline while active, unchanged datum ownership/depth, proper End unwinding, sticky fault reason and cleanup refusal without frame consumption. Cleanup deliberately faults a nonbusy active scope. Cleanup completion is not whole-owner retirement authority; actual hardware/other-owner observations remain required.

Each level1/2 run discharges24instance proof checks plus54flow/init/termination checks, and7model proof checks plus17flow/init/termination checks. These are modeled configured-instance properties, not proof of arbitrary callback semantics, physical effects, time accuracy, exception/reentry behavior, all depth values, native service or complete AML. Host regressions cover separate reentry/uncertainty/actual-Core behavior.
