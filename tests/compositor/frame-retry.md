# Whole-frame retry recovery

A renderer can complete an upload without producing a publishable scene. The
new Retry completion result means all renderer readers and writers are known
quiescent and Desktop must recapture the scene. It never means display retirement.
Unknown ownership still takes Unsafe and must not enter this recovery path.

Desktop retires the matching writer using the existing proved Failed_Quiescent
transition, marks only that target for full repaint, restores the captured frame
into incoming damage, then acquires a fresh writer ticket. No presentation token
is allocated, no Display request is sent, and the retained front is unchanged.
Compositor_Damage.Restore preserves every old and fresh region; it uses the
existing bounded damage envelope and no pixel storage or heap allocation.

Selected damage-policy proof: 34 checks (11 flow, 23 prover), none unproved or
justified. Hosted lifecycle regression: 1,000 retries and 32,000 fresh updates,
held-front protection, failed-slot full repair and rejection of failed-frame
publication. Native fault fixture deliberately corrupts one pixel after software
completion and returns Retry on frame three of each output. It checks restored
old/fresh damage, unchanged presentation token/front, fresh writer identity and
subsequent publication after recapture, then runs the normal mixed-output tests.
Native primary, fractional scaling, arrangement and Desktop interaction groups
pass. These are CuBit/QEMU software fault tests, not hardware GPU timing results.

The caller still trusts the renderer's quiescence report and mapping authority.
The legacy main loop is an audited integration boundary. Cold glyph uploads in
Desktop_GPU_Scene already return Retry after reader retirement, but connecting
that owner to the facade and authenticated client/Display GPU images remains work.


Run hosted checks with `gprbuild -P tests/compositor/frame_retry.gpr`, then
`tests/compositor/build/frame-retry/frame_retry_tests`, inside Nix. Run selected
proof with `gnatprove -P tests/compositor/frame_retry.gpr -u compositor_damage.adb`.
For native faults, use `build-desktop-completion-fixture.py MESA_BUILD --retry`
then `run-desktop-completion-fixture.py FIXTURE_DIR`, under the shared build lock
in Nix. The runner restores the previously staged Desktop. Exact source hashes,
proof, native serial output and restoration records accompany
`build/frame-retry-published.json`.
