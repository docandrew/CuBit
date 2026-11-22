pragma SPARK_Mode (On);
with Timer_Heap;
package Timers_Small is new Timer_Heap (Max_Timers => 16);
