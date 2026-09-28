--  Test stand-in for the kernel's Time (only what Process.Queues reads).
with Interfaces;
package Time is
    tscPerDuration : Interfaces.Unsigned_64 := 1;
end Time;
