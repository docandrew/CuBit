pragma Ada_2022;
with Test_Heap;package body Devmgr_Heap is
 function Allocate(Bytes,Alignment:Interfaces.Unsigned_64)return System.Address is (Test_Heap.Allocate(Bytes,Alignment));
 procedure Release(Block:System.Address) is begin Test_Heap.Release(Block);end;end;
