pragma SPARK_Mode (On);
with Chunk_Pool;
--  A realistic size, proved only (no pool object is declared).
package Pool_Full is new Chunk_Pool
  (Chunk_Count => 65_536, Chunk_Size => 4_096, Owner_Count => 65_536);
