pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

with acpi;
with Boot_Modules;
with Config;
with Multiboot;
with TextIO; use TextIO;
with Virtmem;

package body Physical_Range_Map with SPARK_Mode => Off is

   package PR renames Physical_Ranges;

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
      + acpi.MAX_IOAPICS + Fixed_Spans > PR.Max_Spans,
      "Physical classification cannot hold every boot description");

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
      PR.Seal (Kernel_Map);

      print ("Physical ranges: ");
      print (Integer (Kernel_Map.Count));
      if Kernel_Map.Complete then
         println (" spans, sealed");
      else
         println (" spans, INCOMPLETE: no device-memory authority");
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
