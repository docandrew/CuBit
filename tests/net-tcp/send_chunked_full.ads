pragma SPARK_Mode (On);
with Pool_Full;
with Chunked_Send_Queue;
--  A realistic size (64 chunks of 4 KiB), proved only.
package Send_Chunked_Full is new Chunked_Send_Queue (Chunks => Pool_Full, Max_Chunks => 64);
