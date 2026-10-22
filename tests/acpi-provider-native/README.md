# Provider native-adapter hosted validation

This opt-in suite validates the ACPI-owned provider callback bridge. It does not launch a native provider or change the ACPI server build. The bridge owns a process-lifetime immutable catalog and allocation, uses finite GETTIME deadlines and read-only capability grants, and retains uncertain creation backing without reset or free. NativeBlocks retains only its previously reviewed unused-context cleanup.

Run in the repository's pinned Nix environment with TMPDIR=/home/doc/cubit-build-tmp:

    python3 -B tests/acpi-provider-native/run.py --validate-only
    python3 -B tests/acpi-provider-native/run.py --mode release
    python3 -B tests/acpi-provider-native/run.py --mode checked

The default runs all seven selections using fourteen profile-specific projects: mechanics (12 fresh processes), composition (8), multipage (1), notify_order (2), native_notify (13), provider_api and adapter_api (compile-only against actual runtime/allocator specs, no stubs or linking). Each selection has a closed source inventory with strict -gnatwa/-gnatwe. Limits remain180s/build,30s/process,64MiB stack,1MiB log,nice19/j1. Source mirrors must match pinned repository originals. Runtime stubs are separate from actual-API projects.

Fresh release worker15996 and checked worker7365 passed all seven selections on the current bridge:664checks/profile across36 runtime processes and43 zero build/runtime stages/profile. The count comprises mechanics148, composition/multipage98, fixture ordering58 and production native-notification wrappers360. Both real-API compile-only selections passed each profile. Receipt provider-native-notify-final-gy25xbe6/review.json records source, tool, runtime, binary, compiler/ALI and artifact audits. These are fresh changed-source results, not carried predecessor credit.

## Required startup notification API

Initialize the singleton with Startup => Required when startup notifications are required. Disabled is the explicit default upload-only policy for existing callers. Required blocks Start, import and Finish until Configure_Interest returns Interest_Armed. Configure_Interest consumes its one attempt before querying Idle/current revision, then sends the interest mutation once. Unknown, rejected, malformed or deadline-failed outcomes permanently block Required upload; do not replay configuration or infer recovery from diagnostics. Inspect Last_Interest_Error for a classified remote rejection.

After arming, run the existing Snapshot_Provider.Driver with the bridge's Exchange, Import_Table and Release_Provider_Grant callbacks. Its fresh queries observe the configuration revision; do not interleave another interest mutation during upload. Bootstrap.Start preserves requested interest. Module notifications remain disabled; interest is enabled after member initialization and before device _INI.

Only an exact, known successful Finish admits Start_Notifications. A server that appears Complete after a lost Finish reply does not admit receipt mutations. Incomplete and malformed Finish responses also confer no admission. Read_Metrics diagnostics and grant cleanup never reset the upload stop latch. Accepted revoke is not confirmed retirement; pending receiver acquisition keeps backing alive. Uncertain creation remains quarantined for process lifetime.

Start_Notifications creates a session in the bridge's private Notification_Policy state. Advance_Notifications performs at most one finite call using the existing endpoint and deadline helper. Has_Notification and Copy_Notification expose a copied observation. The trusted broker must actually deliver it, then call Confirm_Notification_Delivery with its exact Notification_Session and receipt ID. No endpoint or raw ACK is exported to handlers. Advance then sends the policy-generated ACK. Unknown claim responses resynchronize; unknown ACK responses retry the same confirmed receipt, never a later one. Malformed replies retire that consumer session. Calling Start_Notifications again uses the existing monotonic lifetime, never a reset or endpoint retarget.

Notification_Path_Limit is32 segments; Notification_Session_Limit and Notification_Request_Limit are ACPI_Requests.Max_Revision. Counter exhaustion fails closed. All operations use finite Query_And_Start windows with overflow rejection. Unknown receipt operations preserve the consumer's retry policy; they never clear Mutation_Stopped, Start_Sent or Finish_Sent and cannot reopen upload. These controls enforce the trusted bridge API, not behavior of an arbitrary holder bypassing it with raw IPC.

## Evidence boundaries

Composition uses the real Driver, bridge, NativeBlocks, Endpoint, Requests/Bootstrap/Core and consumer. IPC, allocator, grant lifecycle, clock and endpoint authority are modeled boundaries. Normal replies come from real dispatch; loss/malformed faults are explicit. Receiver return retries run outside provider callbacks. The multipage case imports a checksummed4135-byte Description table in2pages and checks immutable bytes and zero padding while held.

notify_order retains the earlier fixture-local gate; native_notify exercises the production wrappers and covers pre-arm denial, successful arming/upload/delivery, lost/malformed/denied interest, lost query and deadline failure, lost/malformed Finish, lost claim/ACK protecting a newer receipt, malformed receipt, deadline retry and reconnect. Both reuse exact constructed Migration_Tables startup AML from notification_consumer_tests.adb; no normal-ASL compiler provenance is claimed.

Historical receipts in evidence/ retain their original paths and scope. Earlier portable19206 covered246release checks and the API compiles; predecessors67312/8976 supplied checked mechanics/composition evidence. Ordering87565/18513 supplied58/profile on the predecessor bridge. Those historical results are not substituted for the fresh664/profile gate. Original31402/27005/3402 compile failures and58910 private metadata setup failure remain preserved.

Native prerequisites are unresolved: a trusted launcher must bind the provider stamp/offered endpoint to the actual grant owner; an authorized export must supply the complete immutable firmware catalog; the provider executable must link the allocator/runtime and orchestrate the API and broker delivery. The suite creates no physical/PID authority and does not validate kernel grants, native QEMU startup or concurrent delivery. No launcher/CCL/kernel/runtime/logger changes or full-proof claim are made.
