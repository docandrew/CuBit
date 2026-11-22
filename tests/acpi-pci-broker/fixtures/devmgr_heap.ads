pragma Ada_2022;
with System;with Interfaces;package Devmgr_Heap is
 function Allocate(Bytes,Alignment:Interfaces.Unsigned_64)return System.Address;
 procedure Release(Block:System.Address);end;
