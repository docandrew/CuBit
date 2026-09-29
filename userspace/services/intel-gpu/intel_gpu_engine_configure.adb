with Intel_GPU_ADLN_Engine_Settings;
with Intel_GPU_ADLN_MOCS;
package body Intel_GPU_Engine_Configure is
   use Interfaces;
   package Settings renames Intel_GPU_ADLN_Engine_Settings;
   procedure Configure
     (Object : in out Attempt;
      Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Engine : Intel_GPU_ADLN_Inventory.Engine; Status : out Result)
   is
      Plan : constant Settings.Settings_Plan := Settings.Build
        (Inventory, Engine, Intel_GPU_ADLN_MOCS.Uncached_Index);
      Old, Raw, Value : Unsigned_32;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Started then return; end if;
      Object.Started := True;
      if Plan.Count = 0 then return; end if;
      for I in 1 .. Plan.Count loop
         declare Item : constant Settings.Setting := Plan.Entries (I); begin
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            Old := 0;
            if not Item.Masked_Write then
               Old := Read32 (Item.Offset, Item.CPU_Steered);
               if not Owner_Ready then Status := Ownership_Lost; return; end if;
               if Old = Unsigned_32'Last then Status := Read_Failed; return; end if;
            end if;
            Value := Settings.Write_Value (Item, Old);
            Write32 (Item.Offset, Value, Item.CPU_Steered, OK);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if not OK then Status := Write_Failed; return; end if;
            Raw := Read32 (Item.Offset, Item.CPU_Steered);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if Raw = Unsigned_32'Last or else (Raw and Item.Mask) /= Item.Value then
               Status := Readback_Failed; return;
            end if;
         end;
      end loop;
      Status := Ready;
   end Configure;
end Intel_GPU_Engine_Configure;
