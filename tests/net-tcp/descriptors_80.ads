pragma SPARK_Mode;
--  The proved descriptor pool at virtio-net's transmit size.
with Descriptor_Pool;
package Descriptors_80 is new Descriptor_Pool (Count => 80);
