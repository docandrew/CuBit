pragma Ada_2022;
with System;
with System.Storage_Elements;
with Interfaces;
-- Internal metadata, never user handles or authority. Caller serializes all
-- operations. Allocate must return fresh disjoint storage retained forever.
-- Constant-size block initialization; no capacity-sized arrays or searches.
generic
   type Element is private;
   Records_Per_Block : Positive;
   with function Allocate
     (Bytes, Alignment : System.Storage_Elements.Storage_Count)
      return System.Address;
   Allocation_Granule : Positive := 4096;
package DMA_Record_Blocks is
   type Node is private;
   type Reference is access all Node;
   type Pool is limited private;
   type List is limited private;
   type Result is (Ready, Metadata_Quota, No_Memory, Invalid_Backing);
   Metadata_Error : exception;
   function Block_Bytes return Interfaces.Unsigned_64;
   function Metadata_Bytes (Object : Pool) return Interfaces.Unsigned_64;
   function Value (Item : not null Reference) return Element;
   -- Fill a reserved, unpublished record once physical allocation succeeds.
   procedure Set_Value (Item : not null Reference; Data : Element);
   function Empty (Object : List) return Boolean;
   procedure Reserve
     (Object : in out Pool; Initial : Element;
      Byte_Limit : Interfaces.Unsigned_64; Item : out Reference;
      Status : out Result);
   -- No backing is freed by these metadata operations. Release is permitted
   -- only after allocation rollback or confirmed physical retirement.
   procedure Release (Object : in out Pool; Item : in out Reference);
   procedure Push (Object : in out List; Item : not null Reference);
   procedure Pop (Object : in out List; Item : out Reference);
   -- O(1) move of all records, e.g. owner death into retained quarantine.
   procedure Move (Source, Target : in out List);
private
   type Node is record
      Data : Element;
      Next : Reference := null;
      Leased, Linked : Boolean := False;
      Owner : System.Address := System.Null_Address;
   end record;
   type Pool is limited record
      Free : Reference := null;
      Bytes : Interfaces.Unsigned_64 := 0;
   end record;
   type List is limited record
      First, Last : Reference := null;
   end record;
end DMA_Record_Blocks;
