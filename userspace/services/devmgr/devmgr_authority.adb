------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  devmgr's authority - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.IPC;
with Devmgr_Memory_Claims;

package body Devmgr_Authority is

   package BR renames CuBit.Handles.Boot_Roots;
   use type H.ABI.Kind;

   Held : BR.Roots;

   package MC renames Devmgr_Memory_Claims;
   package AMC renames CuBit.ACPI_Memory_Classes;
   pragma Compile_Time_Error
     (MC.Maximum_Claims /= BR.Maximum_Firmware_Roots,
      "one ACPI claim per firmware-memory root");
   Claims : MC.Ledger;
   Windows : array (BR.Firmware_Root_Index) of ACPI_Window;
   Window_Count : BR.Firmware_Root_Count := 0;
   Refused : Natural := 0;

   type Extent is record
      First, Bytes : Unsigned_64 := 0;
   end record;
   Extents : array (BR.Root_Index) of Extent;

   procedure Initialize is
   begin
      Held := BR.Get;
      Extents := [others => (others => <>)];
      if not Held.Valid then
         return;
      end if;
      for R in 1 .. Held.Count loop
         declare
            D : constant CuBit.Handles.IPC.Description :=
              CuBit.Handles.IPC.Describe (Held.Memory (R));
         begin
            if D.Current and then D.Kind = H.ABI.Device_Memory then
               Extents (R) := (First => D.Object, Bytes => D.Parameter);
            end if;
         end;
      end loop;
      --  The firmware-memory roots: ACPI's before any driver exists.
      MC.Initialize (Claims);
      Windows := [others => (others => <>)];
      Window_Count := 0;
      Refused := 0;
      for R in 1 .. Held.Firmware_Count loop
         declare
            D : constant CuBit.Handles.IPC.Description :=
              CuBit.Handles.IPC.Describe (Held.Firmware (R));
            Granted : Boolean := False;
         begin
            --  NVS and the FACS page come read-write, PCI configuration
            --  read-only (its kernel root has no write right): each with
            --  exactly its kind's rights, and grant.
            if D.Current and then D.Kind = H.ABI.Device_Memory
              and then AMC.Firmware_Tag (D.Authority_Tag)
              and then D.Held.Read and then D.Held.Grant
              and then D.Held.Write = AMC.Writable_Of (AMC.Kind_Of (D.Authority_Tag))
            then
               MC.Claim_ACPI (Claims, D.Object, D.Parameter, Granted);
            end if;
            if Granted then
               Window_Count := Window_Count + 1;
               Windows (Window_Count) :=
                 (Memory => Held.Firmware (R), First => D.Object, Bytes => D.Parameter,
                  Kind => AMC.Kind_Of (D.Authority_Tag));
            else
               Refused := Refused + 1;
            end if;
         end;
      end loop;
   end Initialize;

   function ACPI_Window_Count return Natural is (Window_Count);
   function ACPI_Window_At (K : Positive) return ACPI_Window is (Windows (K));
   function ACPI_Windows_Refused return Natural is (Refused);

   function Valid return Boolean is (Held.Valid);
   function Roots return BR.Roots is (Held);

   function Device_Region (Physical, Bytes : Unsigned_64) return H.Device_Memory is
   begin
      if Bytes = 0 or else Physical > Unsigned_64'Last - Bytes
        or else not MC.Clear_Of_ACPI (Claims, Physical, Bytes)
      then
         return H.No_Device_Memory;
      end if;
      for R in 1 .. Held.Count loop
         if Extents (R).Bytes /= 0
           and then Physical >= Extents (R).First
           and then Physical - Extents (R).First <= Extents (R).Bytes
           and then Bytes <= Extents (R).Bytes - (Physical - Extents (R).First)
         then
            return Held.Memory (R);
         end if;
      end loop;
      return H.No_Device_Memory;
   end Device_Region;

   function Device_Copy (Physical, Bytes : Unsigned_64; Kept : H.Rights) return H.Derivation is
     (H.Narrowed (Device_Region (Physical, Bytes), Kept, Physical, Bytes));

end Devmgr_Authority;
