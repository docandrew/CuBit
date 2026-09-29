with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
package Intel_GPU_ADLN_Engine_Settings with SPARK_Mode is
   subtype MOCS_Index is Natural range 0 .. 63;
   type Setting is record
      Offset, Mask, Value : Unsigned_32 := 0;
      Masked_Write, CPU_Steered : Boolean := False;
   end record;
   type Settings_Array is array (Positive range 1 .. 9) of Setting;
   type Settings_Plan is record
      Count : Natural range 0 .. 9 := 0;
      Entries : Settings_Array := [others => <>];
   end record;
   function Build (Description : Intel_GPU_ADLN_Inventory.Inventory;
                   Item : Intel_GPU_ADLN_Inventory.Engine;
                   Uncached_Index : MOCS_Index) return Settings_Plan;
   -- ADL-N engine-domain settings only (pinned Linux v6.16), not GT/context
   -- workarounds. Caller must establish the MOCS table/uncached index first.
   -- CPU_Steered describes applying the setting; GuC saves ALL these entries
   -- with explicit steering, even ordinary CPU MMIO registers.
   function Write_Value (Item : Setting; Previous : Unsigned_32) return Unsigned_32
     with Pre => (Item.Value and not Item.Mask) = 0 and then
       (not Item.Masked_Write or else Item.Mask <= 16#FFFF#);
   -- Pure calculation. A normal write requires a valid, owned fresh read;
   -- callers must not feed failed all-ones reads into read/modify/write.
end Intel_GPU_ADLN_Engine_Settings;
