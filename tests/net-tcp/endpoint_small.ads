pragma SPARK_Mode (On);
with Pool_Small;
with Send_Chunked_Small;
with Receive_Queue_64;
with TCP_Endpoint;
package Endpoint_Small is new TCP_Endpoint
  (Chunks => Pool_Small, Sends => Send_Chunked_Small, Receives => Receive_Queue_64);
