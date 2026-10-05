# Mesa device startup and context retirement

`Vulkan_Device_Owner` connects the existing `Mesa_Service` owner to the proved
context/submission owners. It makes one startup attempt. A zero slot means no
admitted render endpoint and performs no Mesa call. Nonzero slots must come
from the caller's manifest binding and completed admission, never a demo's
numeric slot or a CPU mapping grant.

The SPARK controller retains an accepted Mesa owner even if device/context
creation fails. It matches the context identity before retirement, closes a
live context only through its existing child/source/submission gate, and calls
Mesa close only after context retirement or known-clean initialization failure.
Pending Mesa close retains the owner and allows one poll per caller invocation;
unsafe retirement is sticky. No retry, wait loop, allocation, pixel copy or
device replacement is introduced by the controller.

`Check_Health` now calls the existing Mesa session-health operation only while
the controller is ready. Any failed observation changes readiness to sticky
quarantine without releasing the device, context, children or pending work.
Later checks cannot re-enable it or replay startup. Software, fresh, closing,
retired and quarantined controllers perform no health IPC. A successful check
is only an observation, never a GPU fence or a lease protecting future work.
The native status operation can block: the eventual Desktop integration must
schedule it outside input dispatch and must not query per drawing primitive.

`Vulkan_Device_FFI` is the narrow SPARK-off adapter to `Mesa_Service`. A small
C helper retains a copy of the borrowed device view and zeroed context metadata
in process-static storage; it does not create another device or acquire
authority. Context initialization remains in `Vulkan_Context_Owner`. The
`Global => null` contracts abstract this private foreign storage, rather than
claiming that the foreign calls have no side effects.

## Evidence

- Hosted startup/retirement matrix covers accepted/rejected ownership, missing
  descriptions, all context creation outcomes and all device retirement outcomes.
  Additional tests cover no-authority startup, repeat attempts, retained child
  tokens, foreign context/submission, retained sources, recording, pending GPU
  completion and unsafe context teardown. The actual controller is unchanged;
  only device/context/submission boundaries are substituted.
- `build/vulkan-device-owner-tests-r4.log`: PASS.
- `build/vulkan-device-owner-proof-r1.log` and the GPR's `gnatprove.out`: 584
  checks including dependencies, none unproved or justified.
- `build/vulkan-device-native-xe_j6dfm/result.json`: real native Mesa Ada adapter,
  policy and C storage compiled against copied source/runtime inputs, no root
  drift. This is a component compile, not device execution.
- `tools/build_desktop_vulkan_link.py --device-startup-probe` additionally
  instantiates the controller in a private Desktop main. The test-only probe
  requires software selection and retirement with slot zero; the matching
  native boot runner requires its explicit PASS marker before menu checks.
- `build/desktop-device-startup-r1/result.json`: full native Desktop link,
  SHA256 `0568eae57235589aacbaede75fcd35f021ed4ffed26fbadce265c01c014ae03b`,
  no undefined symbols. `build/desktop-device-startup-boot-r1/result.json`:
  native no-authority startup marker and all three keyboard/menu restoration
  cycles PASS with the explicit copied boot seeds. This executes the new
  controller's software branch, not the nonzero-slot Mesa startup branch.
- Health extension: `build/vulkan-device-health-final.log` passes sticky-loss,
  retained-child and pending-GPU cases, recovery-to-success refusal, no repeat
  startup/close, and no IPC outside ready mode, plus the prior device regression.
  `build/vulkan-device-health/obj/health-proof/gnatprove/gnatprove.out` has 11
  checks for the updated controller, zero unproved/justified. The real Mesa
  health adapter and controller compile natively in
  `build/vulkan-device-native-5el56onj`, with no source drift. This is not a
  native hardware device-loss experiment or a measured latency result.

## Boundaries and remaining integration

There must be exactly one uncopied controller, Mesa owner, context and associated
submission state per process. Native pointer validity, the admitted capability,
Vulkan/driver results and actual external-reader retirement are trusted. A
display-held target must retain its context child token until display retirement;
GPU completion alone does not satisfy that requirement. `Mesa_Service` IPC can
block even though the policy adds no loop.

This controller is not yet called by the normal Desktop. The optional-render
manifest spelling, admitted startup call, production target/pipeline child
registration, frame recording and real presentation still need integration.
Software-startup testing does not exercise GPU execution or validate hardware
latency. The policy does not assert that a failed accepted device consumed no
driver resources; it keeps that ownership until retirement is confirmed.
