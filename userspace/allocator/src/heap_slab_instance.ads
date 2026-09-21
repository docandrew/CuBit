pragma SPARK_Mode (On);
with Heap_Slabs;
package Heap_Slab_Instance is new Heap_Slabs (Slab_Count => 256);
