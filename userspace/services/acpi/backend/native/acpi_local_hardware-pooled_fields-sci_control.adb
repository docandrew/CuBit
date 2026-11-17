pragma Ada_2022;
with ACPI_SCI_Exclusion;
package body ACPI_Local_Hardware.Pooled_Fields.SCI_Control is
   use type ACPI_SCI_Exclusion.Outcome;
   function Exclusive (Layout : ACPI_SCI_Registers.Layout) return Boolean is
      Stats : constant Catalog.Statistics := Catalog.Observe (Hardware);
      Header : constant Catalog.Inventory := Catalog.Header (Hardware, Local_Scope);
   begin
      if Field_State /= Owner_Ready or else not Completed or else not Finished
        or else not Has_Anchor or else not Control_Idle or else not EC_Commands.Quiescent
        or else not ACPI_Local_Hardware.Admitting
        or else not Stats.Sealed or else Stats.Working or else Stats.Quarantined
        or else Stats.Pending /= 0 or else not Header.Available
      then return False; end if;
      -- Account for the whole catalog, not just a filtered scope subset.
      if Header.Count /= Stats.Registers or else Stats.Active_Registers /= Stats.Registers then return False; end if;
      for Ordinal in 1 .. Header.Count loop
         declare Item : constant Catalog.Description :=
            Catalog.Describe (Hardware, Local_Scope, Header.Revision, Ordinal);
         begin
            if not Item.Available or else Item.Item.Writable
              or else Item.Item.Register_Authority.Writable then return False; end if;
         end;
      end loop;
      -- Native PCI exposes Read_Data only. SystemMemory and NVS access are
      -- distinct from x86 port IO. Their admitted hardware's faithful resource
      -- descriptions and lack of indirect SCI-control aliases remain trusted
      -- platform assumptions; this check does not infer them from an address.
      -- EC IO is the only other port-writing path in this provider instance.
      return ACPI_SCI_Exclusion.Check (Layout, EC_Info.Status = EC_Ready,
         EC_Info.Control, EC_Info.Data) = ACPI_SCI_Exclusion.Disjoint;
   end Exclusive;
end ACPI_Local_Hardware.Pooled_Fields.SCI_Control;
