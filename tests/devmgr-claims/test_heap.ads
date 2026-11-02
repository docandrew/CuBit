--  The hosted tests' heap for Devmgr_Growable: the C heap, with a byte
--  budget a test can lower to make the next growth fail on purpose.
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;

package Test_Heap is

   --  Blocks larger than this are refused (Unsigned_64'Last: none are).
   Budget : Unsigned_64 := Unsigned_64'Last;
   Refusals : Natural := 0;

   function Allocate (Bytes, Alignment : Unsigned_64) return System.Address;
   procedure Release (Block : System.Address);

end Test_Heap;
