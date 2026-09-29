package body Intel_GPU_ADLN_Engine_Settings with SPARK_Mode is
   use Intel_GPU_ADLN_Inventory;
   function Build (Description : Inventory; Item : Engine;
                   Uncached_Index : MOCS_Index) return Settings_Plan is
      Result : Settings_Plan;
      MOCS : constant Unsigned_32 := Unsigned_32 (Uncached_Index);
   begin
      if not Description.Valid or else not Description.Engines (Item) then
         return Result;
      end if;
      Result.Count := 1;
      -- CMD_CCTL fields hold MOCS values (index<<1), not raw table indices.
      Result.Entries (1) := (Engine_Base (Item) + 16#C4#, 16#3FFF#,
                            MOCS * 256 + MOCS * 2, True, False);
      if Item = Render then
         Result.Count := 9;
         Result.Entries (2) := (16#B004#, 16#80#, 0, False, False);
         -- Merge indirect-state override and ENABLE_SMALLPL.
         Result.Entries (3) := (16#E18C#, 16#8001#, 16#8001#, True, True);
         Result.Entries (4) := (16#20EC#, 2, 2, True, False);
         -- Merge early-read disable and push-constant dereference hold disable.
         Result.Entries (5) := (16#E4F4#, 16#4100#, 16#4100#, True, True);
         Result.Entries (6) := (16#20A0#, 16#80000#, 16#80000#, False, False);
         Result.Entries (7) := (16#E48C#, 16#200#, 16#200#, True, True);
         Result.Entries (8) := (16#2050#, 16#1080#, 16#1080#, True, False);
         Result.Entries (9) := (16#20E0#, 16#4000#, 16#4000#, True, False);
      end if;
      return Result;
   end Build;
   function Write_Value (Item : Setting; Previous : Unsigned_32) return Unsigned_32 is
     (if Item.Masked_Write then Shift_Left (Item.Mask, 16) or Item.Value
      else (Previous and not Item.Mask) or Item.Value);
end Intel_GPU_ADLN_Engine_Settings;
