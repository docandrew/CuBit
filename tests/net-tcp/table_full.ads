pragma SPARK_Mode (On);
with Connection_Table;
--  A realistic size, proved only (no table object is declared).
package Table_Full is new Connection_Table
  (Max_Connections => 262_144, Bucket_Count => 65_536, Bucket_Size => 8);
