pragma SPARK_Mode (On);
with TCP_Send_Queue;
package Send_Queue_256k is new TCP_Send_Queue (262_144);
