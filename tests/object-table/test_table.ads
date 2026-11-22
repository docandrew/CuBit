--  Hosted instance of Object_Table: pages from the C allocator, poisoned on
--  free; a spinlock built from an atomic flag.
with System;
with Interfaces; use Interfaces;
with Object_Table;

package Test_Table is
   Tag_Multiplier : constant := 16#1_0001#;

   type Record_Type is record
      Tag    : Unsigned_64 := 0;   --  0 (reset) or Id * Tag_Multiplier
      Filler : Unsigned_64 := 0;
   end record;

   procedure Reset (E : in out Record_Type);
   procedure Alloc_Page (Page_Bytes : Natural; Addr : out System.Address);
   procedure Free_Page (Page_Bytes : Natural; Addr : System.Address);
   procedure Lock;
   procedure Unlock;

   Pages_Allocated, Pages_Freed : Natural := 0 with Atomic;

   package Table is new Object_Table
     (Element          => Record_Type,
      Max_Id           => 256,
      Entries_Per_Page => 8,
      Reserved_Last    => 4,
      Reset            => Reset,
      Alloc_Page       => Alloc_Page,
      Free_Page        => Free_Page,
      Lock             => Lock,
      Unlock           => Unlock);
end Test_Table;
