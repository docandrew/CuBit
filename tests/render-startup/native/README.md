# Optional-render native startup fixture

This extends the production `render-launch-policy` CuBit/QEMU regression with
opt-in software startup and one fresh-child retry. It uses the real procmgr,
capability inspection, broker admission and child stop/resume syscalls. QEMU has
no Intel render provider; it cannot establish successful GPU admission or
hardware rendering/presentation performance.

| Executable | Request / approval | Required result |
| --- | --- | --- |
| render-denied | Required, unapproved | No submission; stopped without running |
| render-unavailable | Required, approved | One submission; stopped without running |
| render-software | Optional, unapproved | No submission; runs with empty render slot |
| render-fallback | Optional, approved | Failed GPU child stopped; one fresh software child runs |
| render-occupied | Optional, unapproved, destination is bootstrap process slot | Stopped without running; no retry |
| render-invalid | Unknown request flags | Stopped without running; no retry |

The software fixture inspects slot 25 and emits its kernel-captured incarnation
only if the slot is empty and self-identity capture succeeds. The observer
matches those identities to procmgr's accepted software attempts. It rejects
execution of a failed GPU child or an occupied-slot child, reused incarnations,
extra submissions, extra retries, cleanup errors and missing Devices startup.
Eleven synthetic faulty traces exercise the observer's negative controls.

`build.sh` encodes canonical `.cubit.caps` bytes directly rather than depending
on new CCL syntax. Request type 11, rights 3, param0 0 means required rendering;
param0 1 opts into optional rendering. Param1 and the reserved byte remain zero.
The optional destination is 25; the occupied negative case uses bootstrap slot
3. Unknown flags are invalid. Executable metadata requests authority; it never
approves it. The CCL compiler spelling is a separate owner integration request.

Run from the repository root inside Nix, with the shared build lock held:

```sh
flock --exclusive --nonblock coordination/build.lock \
  env QEMU_MEMORY=1G nix develop -c bash -c '
    set -e
    make -C kernel procmgr devmgr devices render-launch-policy
    bash tests/render-startup/native/build.sh
    CUBIT_RENDER_OPTIONAL_TEST=1 bash tests/headless/run.sh \
      --test render-launch-policy --accel tcg,thread=multi --cpus 4 \
      --timeout 90 --serial /tmp/cubit-render-optional.serial.log --keep-logs
  '
```

The default `render-launch-policy` test still selects its original two mandatory
sentinels. Optional fixture binaries are test artifacts, not production startup
entries. Desktop's manifest is unchanged. The test uses a disposable copy of the
headless base disk.

The startup policy's SPARK proof does not prove the foreign inspection, stop or
resume operations. A stop acknowledgment does not retire GPU resources. This
fixture covers unavailable-provider rejection, not late successful grants,
physical GPU faults, confirmed GPU quiescence or device reset recovery. Those
require separate broker/hardware tests. The runtime retry gate also requires a
new valid generation/PID identity; identical PIDs with different generations are
valid fresh children.

On 2026-10-02 the corrected fixture passed the 90-second native gate with four
TCG CPUs and 1 GiB. It observed four denied attempts, five successful child-stop
requests, two admission submissions, two executed software children and exactly
one retry. The first software child was incarnation 12884901920; the failed
approved GPU child was 17179869216, and its software retry was 4294967329.
Both software applications confirmed empty render slots; Devices then opened.
See `../build/evidence/optional-corrected.log` and its serial log. The first run
is retained as failed evidence: the ELF loader rejected its executable-stack
fixture, and the runner initially failed to propagate the observer's failure.
The fixture now links a non-executable stack, and two checks exercise the actual
shell guard with good and bad traces as well as the 11 oracle negative controls.
