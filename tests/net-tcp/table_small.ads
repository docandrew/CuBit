pragma SPARK_Mode (On);
with Connection_Table;
package Table_Small is new Connection_Table
  (Max_Connections => 8, Bucket_Count => 4, Bucket_Size => 2);
