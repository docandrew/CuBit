# Native ACPI entry

`component.ccl` declares `acpi.svc`, launched by the process manager. Its typed
manifest requires a sealed firmware snapshot reader, logstore, and the ACPI
registration notification. Generated `Startup` checks all three at elaboration.
There is no provider endpoint, runtime role/configure exchange or bulk grant.

`ACPI_Native_Instance.Start` latches once, obtains an absolute monotonic startup
deadline in microseconds, and loads directly through `Startup.Tables`. The
snapshot loader checks full-width metadata before converting bounds, constructs
one limited arena in place from count/total/largest, copies bounded chunks into
owned exact-extent buffers, then performs local admission and initialization.
No partial owner reaches registration or the observer runner. CuBit's allocator
logs and exits on native OOM; hosted Storage_Error recovery is not its behavior.

`ACPI_Service` in this directory selects the native clock before the hosted
default in the parent directory. The explicit64MiB stack setting is retained;
semantic checks do not demonstrate whole-call-chain stack fit.

The runner accepts only the shared `ACPI_Protocol.Observer_Tag`, classified from
the received message's kernel stamp. Neither process identity nor message words
grant a role. `CuBit.Refusal_Notes` bounds repeated refusal logging. Observer
requests cannot upload tables, mutate namespace state or execute arbitrary AML.
Idle waits explicitly use Wait_Forever; unexpected asynchronous completions stop
the runner, which submits no asynchronous work. Replies are attempted once.

Bring-up stages use existing logstore logging and serial echo. Snapshot/startup
success is explicitly marked hardware-pending. Device inventory/caches, event
subscriptions, EC/register mediation, named hardware capabilities and native
I2C/GPIO/HID input still require integration and separate native/hardware tests.
No actual hardware identification is invented from the snapshot metadata.
