pragma SPARK_Mode (On);
with Chunk_Pool;
package Pool_Small is new Chunk_Pool (Chunk_Count => 8, Chunk_Size => 16, Owner_Count => 4);
