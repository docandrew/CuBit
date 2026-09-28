pragma SPARK_Mode (On);
with Connection_Table;
--  netstack's own parameters (netstack_service.adb, Conns), proved as used.
package Table_Netstack is new Connection_Table
  (Max_Connections => 64, Bucket_Count => 64, Bucket_Size => 8);
