with Interfaces; use Interfaces;
package Intel_GPU_ADS_Regset with SPARK_Mode is
   -- Save/restore entries only. Explicit-value/restore-only workarounds need
   -- separate admission; callers cannot smuggle arbitrary firmware flags here.
   subtype Steering_ID is Natural range 0 .. 15;
   type Register_Entry is record
      Offset : Unsigned_32 := 0;
      Masked, Steered : Boolean := False;
      Group_ID, Instance_ID : Steering_ID := 0;
   end record;
   Capacity : constant := 256;
   subtype Entry_Count is Natural range 0 .. Capacity;
   type Entry_Array is array (Positive range 1 .. Capacity) of Register_Entry;
   type Register_List is record
      Count : Entry_Count := 0;
      Entries : Entry_Array := [others => <>];
   end record;
   type Add_Result is (Added, Already_Present, Conflicting_Entry,
                       Invalid_Entry, No_Space);
   function Valid (Item : Register_Entry; MMIO_Bytes : Unsigned_32) return Boolean is
     (MMIO_Bytes >= 4 and then Item.Offset mod 4 = 0 and then
      Item.Offset <= MMIO_Bytes - 4 and then
      (Item.Steered or else (Item.Group_ID = 0 and Item.Instance_ID = 0)));
   procedure Add (List : in out Register_List; Item : Register_Entry;
                  MMIO_Bytes : Unsigned_32; Status : out Add_Result)
     with Post => (if Status /= Added then List = List'Old);
   -- Add keeps a list built from the empty default sorted by offset. Identical
   -- duplicates are harmless; conflicting flags fail rather than depend on order.
   type Wire_Entry is array (Natural range 0 .. 15) of Unsigned_8;
   function Encode (Item : Register_Entry) return Wire_Entry;
   -- Packed guc_mmio_reg: offset/value/flags/mask. Value and mask stay zero.
   -- Encoding is not MMIO authority, steering validation or a complete regset.
end Intel_GPU_ADS_Regset;
