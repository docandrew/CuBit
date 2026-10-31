# EC query publication and AML readback — hosted integration

Run run.sh in Nix with TMPDIR=/home/doc/cubit-build-tmp. --source-root DIR
selects an explicitly pinned source tree; --validate-only verifies pins without
compilation. Outputs are disjoint temporary directories. Builds use j1/nice19,
180-second compile and30-second runtime limits,64MiB stack,and1MiB logs.
Both checked and release profiles run.

The actual EC engine/catalog,retained event publisher,AML method owner,Core,
Field provider,native saved-reply adapter and lid notification path are joined.
Hardware,clock,continuous ownership,authenticated IPC metadata and enqueue
callbacks remain modeled. A completed query is retained before a later enqueue;
the handler runs only after publication returns and reads an EC Field through
the saved-reply backend. Tests check busy arbitration,backpressure without new
hardware queries,bounded delivery episodes,explicit same-notice retry,zero/no
event suppression,safe failed-query resumption and quarantined unknown delivery.
They also retain previous geometry/rights/partial-write/lost-reply regressions.

Run oracle.sh with the printed output directory/ec-fields.aml to independently
execute _Q01 in ACPICA. It uses default simulated OperationRegion access,zero
fill,disabled startup STA/INI and disabled return repair. Expected EVCT,EVST,
_LID observations are1,0,0. This validates handler semantics only,not physical
EC protocol,IRQ,publication or native IPC. Normal iASL compilation is required.

An unknown enqueue may already have committed; it retains the exact notice and
allows no automatic retry,new query or reset. Receiver-side authentication,
route-to-scope binding and cross-delivery deduplication remain native integration
work. The producer has no wire ABI and grants no source/hardware authority.
Observe reports catalog recovery independently of retained publication.

Results:1637joined and1063newgeometry checks/profile,plus original datum162373
and plan686. No new formal proof,native execution or hardware acceptance.
Native admission/leases,GlobalLock,launch,clock/scheduling,_REG/provider/GPE
wiring,delivery reconciliation and the full verified AML/service goal remain
incomplete.
