pragma SPARK_Mode (On);
with Futex_Queues;
with Futex_Keys;
package Futex_Queues_Large is new Futex_Queues (Capacity => Futex_Keys.Max_Waiter);
