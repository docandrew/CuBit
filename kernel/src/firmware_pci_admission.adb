pragma Ada_2022;
with Interfaces;
with Firmware_Tables.MCFG;
with Firmware_Tables.PCI_Windows;
package body Firmware_PCI_Admission with SPARK_Mode is
   package P renames Physical_Ranges;
   package MC renames Firmware_Tables.MCFG;
   package W renames Firmware_Tables.PCI_Windows;
   use type Interfaces.Unsigned_64;
   procedure Admit (M : in out P.Map; Data : Firmware_Tables.Bytes; Result : out Outcome) is
      Metadata : constant MC.Table_Metadata := MC.Decode (Data);
   begin
      Result := Classification_Unavailable;
      if not M.Complete then return; end if;
      Result := Invalid_Table;
      if not Metadata.Valid or else Metadata.Reserved /= 0 then
         P.Invalidate (M); return;
      end if;
      Result := Catalog_Capacity;
      if Metadata.Count > P.Max_PCI_Allocations - M.PCI_Used then
         P.Invalidate (M); return;
      end if;
      for I in 1 .. Metadata.Count loop
         pragma Loop_Invariant (not M.Sealed and then P.Well_Formed (M));
         declare Item : constant MC.Allocation_Result := MC.Read_Allocation (Data, I); begin
            Result := Invalid_Allocation;
            if not Item.Valid or else not W.Valid (Item.Value, P.Address_Limit) then
               P.Invalidate (M); return;
            end if;
            P.Add_PCI_Config (M, Item.Value);
            if not M.Complete then Result := Conflicting_Allocation; return; end if;
         end;
      end loop;
      Result := Admitted;
   end Admit;
end Firmware_PCI_Admission;
