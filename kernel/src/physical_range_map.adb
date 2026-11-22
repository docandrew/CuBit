pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

with acpi;
with Firmware_Tables.MCFG;
with Firmware_Tables.Snapshots;
with Firmware_PCI_Admission;
with Boot_Modules;
with Firmware_Tables.Catalog;
with Firmware_Tables.Remapping;
with Config;
with Multiboot;
with TextIO; use TextIO;
with Virtmem;

package body Physical_Range_Map with SPARK_Mode => Off is

   package PR renames Physical_Ranges;
   use type Firmware_Tables.Catalog.Phase;
   use type acpi.DMAR_Status;

   -- The x86 interrupt-message window: the local APIC registers and every
   -- MSI/MSI-X write target. Architectural, independent of firmware tables.
   Interrupt_Window_First : constant := 16#FEE0_0000#;
   Interrupt_Window_Last  : constant := 16#FEEF_FFFF#;
   -- One page of registers for an I/O APIC or HPET block.
   Register_Page_Last_Offset : constant := PR.Page_Bytes - 1;

   -- Low memory, kernel image, framebuffer, interrupt window, local APIC,
   -- HPET.
   Fixed_Spans : constant := 6;

   pragma Compile_Time_Error
     (Config.MAX_BOOT_MEMORY_REGIONS + Boot_Modules.Maximum_Modules
      + acpi.MAX_IOAPICS + Firmware_Tables.Remapping.Maximum_Units + Fixed_Spans
        > PR.Max_Spans,
      "Physical classification cannot hold every boot description");
   pragma Compile_Time_Error
     (Firmware_Tables.Catalog.Max_Tables > PR.Max_Table_Extents,
      "Physical classification cannot hold every sealed table");

   procedure Add_Register_Page (Physical : Virtmem.PhysAddress) is
   begin
      if Physical /= 0 then
         PR.Add (Kernel_Map,
                 Unsigned_64 (Physical) - Unsigned_64 (Physical) mod PR.Page_Bytes,
                 Unsigned_64 (Physical) - Unsigned_64 (Physical) mod PR.Page_Bytes
                   + Register_Page_Last_Offset,
                 PR.Kernel_MMIO);
      end if;
   end Add_Register_Page;

   -- The classifier can retain at most this many allocation records. A larger
   -- MCFG cannot be represented and invalidates all PCI facts before copying.
   -- Static storage avoids both the boot stack and the kernel's absent heap.
   Max_MCFG_Bytes : constant Positive := Firmware_Tables.MCFG.Fixed_Size
     + PR.Max_PCI_Allocations * Firmware_Tables.MCFG.Allocation_Size;
   PCI_Metadata_Buffer : Firmware_Tables.Bytes (1 .. Max_MCFG_Bytes);
   type PCI_Metadata_Outcome is (Recorded, No_MCFG, Snapshot_Unavailable,
      Descriptor_Unavailable, Duplicate_MCFG, Metadata_Capacity, Copy_Refused, Policy_Refused);
   procedure Print_Reason (Status : PCI_Metadata_Outcome) is
   begin
      case Status is
         when Recorded => println ("RECORDED");
         when No_MCFG => println ("NO_MCFG");
         when Snapshot_Unavailable => println ("SNAPSHOT_UNAVAILABLE");
         when Descriptor_Unavailable => println ("DESCRIPTOR_UNAVAILABLE");
         when Duplicate_MCFG => println ("DUPLICATE_MCFG");
         when Metadata_Capacity => println ("METADATA_CAPACITY");
         when Copy_Refused => println ("COPY_REFUSED");
         when Policy_Refused => println ("POLICY_REFUSED");
      end case;
   end Print_Reason;
   -- Read only the owned sealed snapshot, never original firmware pointers.
   procedure Record_PCI_Allocations (Status : out PCI_Metadata_Outcome) is
      package S renames Firmware_Tables.Snapshots;
      use type S.Phase, S.Metadata_Status, acpi.Table_Copy_Status;
      use type Firmware_PCI_Admission.Outcome;
      Copied : acpi.Table_Copy_Status;
      Result : Firmware_PCI_Admission.Outcome;
      Seen : Boolean := False;
   begin
      Status := No_MCFG;
      if acpi.Table_Snapshot_State /= S.Ready then
         PR.Invalidate (Kernel_Map); Status := Snapshot_Unavailable; return;
      end if;
      for I in 1 .. acpi.Snapshot_Table_Count loop
         declare D : constant S.Metadata_Result := acpi.Describe_Snapshot_Table (I); begin
            if D.Status /= S.Metadata_Available then
               PR.Invalidate (Kernel_Map); Status := Descriptor_Unavailable; return;
            end if;
            if D.Info.Name = "MCFG" then
               if Seen then PR.Invalidate (Kernel_Map); Status := Duplicate_MCFG; return; end if;
               Seen := True;
               if D.Info.Extent > Max_MCFG_Bytes then
                  PR.Invalidate (Kernel_Map); Status := Metadata_Capacity; return;
               end if;
               acpi.Copy_Table (I, PCI_Metadata_Buffer (1 .. D.Info.Extent), Copied);
               if Copied /= acpi.Copied then
                  PR.Invalidate (Kernel_Map); Status := Copy_Refused; return;
               end if;
               Firmware_PCI_Admission.Admit
                 (Kernel_Map, PCI_Metadata_Buffer (1 .. D.Info.Extent), Result);
               if Result /= Firmware_PCI_Admission.Admitted then Status := Policy_Refused; return; end if;
               Status := Recorded;
            end if;
         end;
      end loop;
   end Record_PCI_Allocations;

   procedure Seal_From_Boot is
      Kernel_Last : constant Unsigned_64 :=
        Unsigned_64 (Virtmem.V2P (To_Address (Virtmem.STACK_TOP))) - 1;
   begin
      if Kernel_Map.Sealed then
         return;
      end if;
      PR.Add (Kernel_Map, 0, PR.Low_Memory_Limit - 1, PR.Low_Memory);
      PR.Add (Kernel_Map, Config.MIN_PHYS_ALLOC, Kernel_Last, PR.Kernel_Image);
      Multiboot.Describe_Physical_Ranges (Kernel_Map);
      PR.Add (Kernel_Map, Interrupt_Window_First, Interrupt_Window_Last,
              PR.Kernel_MMIO);
      Add_Register_Page (acpi.lapicAddr);
      for Physical of acpi.ioapicAddrs loop
         Add_Register_Page (Physical);
      end loop;
      if acpi.ioapicOverflow then
         PR.Invalidate (Kernel_Map);
      end if;
      Add_Register_Page (acpi.hpetAddr);
      -- Every DMA remapping unit's register set (IOMMU-001): the kernel
      -- drives them; no process may map them.
      if acpi.dmarStatus = acpi.DMAR_Accepted then
         for U in 1 .. acpi.dmarFacts.Unit_Total loop
            PR.Add (Kernel_Map, acpi.dmarFacts.Units (U).Register_Base,
                    acpi.dmarFacts.Units (U).Register_Base
                      + Unsigned_64 (acpi.dmarFacts.Units (U).Register_Pages) * PR.Page_Bytes - 1,
                    PR.Kernel_MMIO);
         end loop;
      end if;
      -- The FACS page: firmware memory (write-back) when it is a whole
      -- NVS or firmware-reserved page; nothing when the FADT names none.
      if acpi.facsAddr /= 0 then
         PR.Set_Control (Kernel_Map, acpi.facsAddr);
      end if;
      -- All validated MCFG allocations become immutable classification facts.
      -- Enumeration creates RO roots; only devmgr narrows a requested bus.
      declare Status : PCI_Metadata_Outcome; begin
         Record_PCI_Allocations (Status);
         if Status not in Recorded | No_MCFG then
            print ("Physical ranges: PCI metadata refused ");
            Print_Reason (Status);
         end if;
      end;
      -- The sealed ACPI tables (the admitted catalog, root table included):
      -- never firmware data, whatever span holds them. Without the catalog
      -- nothing is firmware data.
      if acpi.Table_Inventory_State = Firmware_Tables.Catalog.Ready then
         for Index in 1 .. acpi.Table_Count loop
            declare
               D : constant Firmware_Tables.Catalog.Descriptor := acpi.Table_Source (Index);
            begin
               PR.Add_Table (Kernel_Map, D.Physical, D.Physical + Unsigned_64 (D.Extent - 1));
            end;
         end loop;
      else
         PR.Tables_Unknown (Kernel_Map);
      end if;
      PR.Seal (Kernel_Map);

      print ("Physical ranges: ");
      print (Integer (Kernel_Map.Count));
      print (" spans, ");
      print (Integer (Kernel_Map.Tables_Used));
      print (" tables");
      if not Kernel_Map.Tables_Complete then
         print (" (incomplete: no firmware data)");
      end if;
      if Kernel_Map.Complete then
         println (", sealed");
      else
         println (", INCOMPLETE: no device-memory authority");
      end if;
   end Seal_From_Boot;

   function Within_Boot_Module (Base, Bytes : Unsigned_64) return Boolean is
   begin
      if not Kernel_Map.Sealed or else Bytes = 0
        or else Bytes - 1 > Unsigned_64'Last - Base
      then
         return False;
      end if;
      for Index in 1 .. Multiboot.Boot_Module_Count loop
         declare
            Item : constant Boot_Modules.Image := Multiboot.Boot_Module (Index);
         begin
            if Base >= Unsigned_64 (Item.First)
              and then Base + (Bytes - 1) < Unsigned_64 (Item.Page_Limit)
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Within_Boot_Module;

end Physical_Range_Map;
