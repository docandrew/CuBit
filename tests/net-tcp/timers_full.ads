pragma SPARK_Mode (On);
with Timer_Heap;
--  A realistic size, proved only (no heap object is declared).
package Timers_Full is new Timer_Heap (Max_Timers => 1_048_576);
