with Intel_GPU_Nonpriv_Registers;
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
         Result.Count := 21;
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
         -- TGL PRM Vol2c-12.21 pp988-989: explicit FORCE_TO_NONPRIV fields.
         -- Cross-check Linux intel_workarounds.c tgl_whitelist_build and
         -- intel_engine_apply_whitelist (v6.19-rc8-185-g2687c848e578).
         -- RCS gets four counter DWORDs RO and three tuning registers RW.
         -- Use individual counters rather than Linux's range4 at 0x2348:
         -- documented range comparison ignores address bits3:2.
         -- Clear all remaining i915-managed slots to RING_NOPID, not zero.
         for Slot in 0 .. 11 loop
            declare
               package NP renames Intel_GPU_Nonpriv_Registers;
               Offset : constant Unsigned_32 :=
                 (case Slot is when 0 .. 3 => 16#2348# + Unsigned_32 (Slot) * 4,
                  when 4 => 16#7010#, when 5 => 16#7018#,
                  when 6 => 16#7304#, when others => 16#2094#);
               Permission : constant NP.Register_Value :=
                 (Address_DWords => NP.Bits_24 (Offset / 4),
                  Access_Selection => (if Slot < 4 then 1 else 0), others => <>);
            begin
               Result.Entries (10 + Slot) :=
                 (NP.Documented_RCS_Offset (Slot), Unsigned_32'Last,
                  NP.Encode (Permission), False, False);
            end;
         end loop;
      end if;
      return Result;
   end Build;
   function Write_Value (Item : Setting; Previous : Unsigned_32) return Unsigned_32 is
     (if Item.Masked_Write then Shift_Left (Item.Mask, 16) or Item.Value
      else (Previous and not Item.Mask) or Item.Value);
end Intel_GPU_ADLN_Engine_Settings;
