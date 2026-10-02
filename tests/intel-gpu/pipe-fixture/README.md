# Native pipe admission fixture

Compiles and instantiates the actual native adapter and power/IRQ helpers.
Only mapping readiness and the clock boundary are substituted. All rejected
pipe/flag/parent combinations are checked, then retried with stronger inputs
to verify the one-shot attempt remains consumed. No successful acquisition is
attempted: the production MMIO addresses must never be accessed on the host.

This tests prerequisite rejection, not hardware power, interrupt delivery,
MMIO ordering, capability authenticity, or successful acquisition. The clock
stub raises if a rejected call incorrectly reaches the timing backend.
