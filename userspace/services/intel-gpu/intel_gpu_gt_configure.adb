with Intel_GPU_ADLN_GT_Settings;
package body Intel_GPU_GT_Configure is
   use Interfaces;
   package Settings renames Intel_GPU_ADLN_GT_Settings;
   function Last_Offset (Object : Attempt) return Unsigned_32 is (Object.Offset);
   function Last_Readback (Object : Attempt) return Unsigned_32 is (Object.Raw);
   procedure Configure
     (Object : in out Attempt;
      Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology; Status : out Result)
   is
      Plan : constant Settings.Plan := Settings.Build (Inventory, Topology);
      Before, Value, Relevant : Unsigned_32;
      OK, Overridden : Boolean := False;
   begin
      Status := Rejected;
      if Object.Started then return; end if;
      Object.Started := True;
      if Plan.Count = 0 then return; end if;
      for I in 1 .. Plan.Count loop
         declare S : constant Settings.Setting := Plan.Items (I); begin
            Object.Offset := S.Offset;
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            Before := Read32 (S.Offset, S.MCR);
            Object.Raw := Before;
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if Before = Unsigned_32'Last then Status := Read_Failed; return; end if;
            Value := (Before and not S.Clear_Mask) or S.Set_Bits;
            Write32 (S.Offset, Value, S.MCR, OK);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if not OK then Status := Write_Failed; return; end if;
            Object.Raw := Read32 (S.Offset, S.MCR);
            if not Owner_Ready then Status := Ownership_Lost; return; end if;
            if Object.Raw = Unsigned_32'Last then Status := Read_Failed; return; end if;
            if (Object.Raw and S.Verify_Mask) /= (Value and S.Verify_Mask) then
               Status := Readback_Failed; return;
            end if;
            Relevant := S.Clear_Mask or S.Set_Bits;
            if S.Verify_Mask = 0 and then
              (Object.Raw and Relevant) /= (Value and Relevant)
            then Overridden := True; end if;
         end;
      end loop;
      Status := (if Overridden then Ready_With_Firmware_Override else Ready);
   end Configure;
end Intel_GPU_GT_Configure;
