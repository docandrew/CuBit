with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Engine_Settings;
package body Intel_GPU_ADLN_Regset with SPARK_Mode is
   use Intel_GPU_ADLN_Inventory;
   use Intel_GPU_ADS_Regset;
   function Build_Common
     (Description : Inventory; Item : Engine;
      Steering : Intel_GPU_ADLN_Steering.Topology;
      MMIO_Bytes : Unsigned_32) return Register_Set is
      Result : Register_Set;
      Base : constant Unsigned_32 := Engine_Base (Item);
      Failed : Boolean := False;
      Perf : constant array (Positive range 1 .. 7) of Unsigned_32 :=
        [16#E458#, 16#E558#, 16#E658#, 16#E758#,
         16#E45C#, 16#E55C#, 16#E65C#];
      procedure Include (Offset : Unsigned_32; Masked, Steered : Boolean)
        with Pre => Steering.Valid
      is
         Entry_Value : Register_Entry := (Offset, Masked, Steered, 0, 0);
         Status : Add_Result;
      begin
         if Failed then return; end if;
         if Steered then
            Entry_Value.Instance_ID := Intel_GPU_ADLN_Steering.Instance (Steering, Offset);
         end if;
         Add (Result.Registers, Entry_Value, MMIO_Bytes, Status);
         Failed := Status /= Added;
      end Include;
   begin
      if not Description.Valid or else not Description.Engines (Item) or else
        not Steering.Valid
      then return Result; end if;
      Include (Base + 16#29C#, True, False); -- RING_MODE_GEN7, not RING_MI_MODE
      Include (Base + 16#80#, False, False); -- RING_HWS_PGA
      Include (Base + 16#A8#, False, False); -- RING_IMR
      for I in 0 .. 11 loop
         Include (Base + 16#4D0# + Unsigned_32 (I * 4), False, False);
      end loop;
      for I in 0 .. 31 loop
         Include (16#B020# + Unsigned_32 (I * 4), False, False);
      end loop;
      for Offset of Perf loop Include (Offset, False, True); end loop;
      if Failed or Result.Registers.Count /= 54 then return (others => <>); end if;
      Result.Ready := True;
      return Result;
   end Build_Common;
   function Build
     (Description : Inventory; Item : Engine;
      Steering : Intel_GPU_ADLN_Steering.Topology;
      MMIO_Bytes : Unsigned_32) return Register_Set is
      Result : Register_Set := Build_Common (Description, Item, Steering, MMIO_Bytes);
      -- Index affects written values, NOT the set of registers or save flags.
      -- No MOCS table selection is made by requesting metadata with index0.
      Settings : constant Intel_GPU_ADLN_Engine_Settings.Settings_Plan :=
        Intel_GPU_ADLN_Engine_Settings.Build (Description, Item, 0);
      Status : Add_Result;
   begin
      if not Result.Ready or else not Steering.Valid or else Settings.Count = 0 then
         return (others => <>);
      end if;
      for I in 1 .. Settings.Count loop
         -- FORCE_TO_NONPRIV slots are already in Build_Common as ordinary
         -- unsteered registers. Do not change their save/restore metadata.
         if Settings.Entries (I).Offset not in 16#24D0# .. 16#24FC# then
         Add (Result.Registers,
              (Offset => Settings.Entries (I).Offset,
               Masked => Settings.Entries (I).Masked_Write,
               Steered => True, Group_ID => 0,
               Instance_ID => Intel_GPU_ADLN_Steering.Instance
                 (Steering, Settings.Entries (I).Offset)), MMIO_Bytes, Status);
         if Status /= Added and Status /= Already_Present then
            return (others => <>);
         end if;
         end if;
      end loop;
      if Result.Registers.Count /= (if Item = Render then 63 else 55) then
         return (others => <>);
      end if;
      return Result;
   end Build;
end Intel_GPU_ADLN_Regset;
