pragma SPARK_Mode (On);
--  Proof instances of the kernel's generic Futex_Queues, at the capacities
--  the kernel uses (Process.Futex): 32-slot buckets and the overflow set.
with Futex_Queues;
package Futex_Queues_Small is new Futex_Queues (Capacity => 32);
