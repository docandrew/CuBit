pragma SPARK_Mode (On);
with Pool_Small;
with Chunked_Send_Queue;
package Send_Chunked_Small is new Chunked_Send_Queue (Chunks => Pool_Small, Max_Chunks => 4);
