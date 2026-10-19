------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's growable tables in the process heap (see the spec).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body Desktop_Heap_Objects is
   use System.Storage_Elements;
   use type System.Address;

   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";

   --  A cache line: tables are scanned whole.
   ALIGNMENT : constant := 64;

   function Allocate (Bytes : Storage_Count) return System.Address is
     (To_Address (Integer_Address (CuAlloc_Allocate (Unsigned_64 (Bytes), ALIGNMENT))));

   procedure Free (Where : System.Address) is
   begin
      if Where /= System.Null_Address then
         CuAlloc_Free (Unsigned_64 (To_Integer (Where)));
      end if;
   end Free;

   procedure Free (Item : in out Reference) is
   begin
      if Item /= null then
         Free (Conversions.To_Address (Item));
         Item := null;
      end if;
   end Free;
end Desktop_Heap_Objects;
