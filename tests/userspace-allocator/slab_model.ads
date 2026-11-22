pragma SPARK_Mode (On);
with Heap_Slabs;
package Slab_Model is new Heap_Slabs (Slab_Count => 4);
