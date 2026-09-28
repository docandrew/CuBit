pragma SPARK_Mode (On);
with TCP_Receive_Queue;
package Receive_Queue_256k is new TCP_Receive_Queue (262_144);
