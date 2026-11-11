------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  devmgr's authority - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.IPC;
with Firmware_Data_ABI;
with Devmgr_Growable;

package body Devmgr_Authority is

   package BR renames CuBit.Handles.Boot_Roots;
   package MC renames Devmgr_Memory_Claims;
   package AMC renames CuBit.ACPI_Memory_Classes;
   use type H.ABI.Kind;
   use type AMC.Memory_Kind;
   use type MC.Claim_Result;
   use type Ledgers.Port_Store.Growth_Status;

   Held : BR.Roots;
   --  A device-memory root's tag: none (Capability_ABI, untagged device
   --  memory).
   Untagged : constant Unsigned_64 := 0;

   type Extent is record
      First, Bytes : Unsigned_64 := 0;
      Memory : H.Device_Memory := H.No_Device_Memory;
   end record;

   package Extent_Store is new Devmgr_Growable (Extent, Devmgr_Heap.Allocate, Devmgr_Heap.Release);
   package Window_Store is new Devmgr_Growable
     (ACPI_Window, Devmgr_Heap.Allocate, Devmgr_Heap.Release);
   package Data_Store is new Devmgr_Growable (Data_Root, Devmgr_Heap.Allocate, Devmgr_Heap.Release);
   use type Window_Store.Growth_Status;
   use type Data_Store.Growth_Status;

   Claims  : Ledgers.Memory_Ledger;
   Extents : Extent_Store.Store;
   Windows : Window_Store.Store;
   PCI_Roots : Window_Store.Store;
   Data    : Data_Store.Store;
   Refused : Natural := 0;
   Device_Failure : Capacity_Report;
   Device_Failed  : Boolean := False;

   procedure Initialize is
   begin
      Held := BR.Get;
      Extent_Store.Clear (Extents);
      Window_Store.Clear (Windows);
      Window_Store.Clear (PCI_Roots);
      Data_Store.Clear (Data);
      Refused := 0;
      if not Held.Valid then
         return;
      end if;
      for R in 1 .. Held.Count loop
         declare
            Memory : constant H.Device_Memory := BR.Device_Root (Held, R);
            D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Memory);
            Report : Extent_Store.Capacity_Report;
         begin
            Extent_Store.Append
              (Extents,
               (if D.Current and then D.Kind = H.ABI.Device_Memory
                  and then D.Authority_Tag = Untagged
                then (First => D.Object, Bytes => D.Parameter, Memory => Memory)
                else (First => 0, Bytes => 0, Memory => H.No_Device_Memory)),
               Report);
         end;
      end loop;
      --  NVS, FACS and PCI roots are claimed before drivers exist. Keep PCI
      --  roots in a separate store for individual read-only bus requests;
      --  data roots remain available for envelope narrowing.
      for R in 1 .. Held.Firmware_Count loop
         declare
            Memory : constant H.Device_Memory := BR.Firmware_Root (Held, R);
            D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Memory);
            Result : MC.Claim_Result := MC.Malformed;
            Report : Capacity_Report;
            Kept : Boolean := False;
         begin
            if D.Current and then D.Kind = H.ABI.Device_Memory
              and then D.Authority_Tag = Firmware_Data_ABI.Firmware_Data_Tag
              and then D.Held.Read and then D.Held.Write and then D.Held.Grant
              and then MC.Whole_Pages (D.Object, D.Parameter)
            then
               declare
                  Grown : Data_Store.Capacity_Report;
               begin
                  Data_Store.Append (Data, (Memory => Memory, First => D.Object,
                                            Bytes => D.Parameter), Grown);
                  Kept := Grown.Status = Data_Store.Grown;
               end;
            --  NVS and the FACS page come read-write, PCI configuration
            --  read-only (its kernel root has no write right): each with
            --  exactly its kind's rights, and grant.
            elsif D.Current and then D.Kind = H.ABI.Device_Memory
              and then AMC.Firmware_Tag (D.Authority_Tag)
              and then D.Held.Read and then D.Held.Grant
              and then D.Held.Write = AMC.Writable_Of (AMC.Kind_Of (D.Authority_Tag))
            then
               Ledgers.Claim (Claims, D.Object, D.Parameter, MC.ACPI_Window, Result, Report);
               if Result = MC.Granted then
                  declare
                     Grown : Window_Store.Capacity_Report;
                  begin
                     if AMC.Kind_Of (D.Authority_Tag) = AMC.PCI_Config then
                        -- Root authority stays in devmgr; a broker request
                        -- later selects one bus and returns a read-only copy.
                        Window_Store.Append
                          (PCI_Roots, (Memory => Memory, First => D.Object,
                            Bytes => D.Parameter, Kind => AMC.PCI_Config), Grown);
                     else
                        Window_Store.Append
                          (Windows, (Memory => Memory, First => D.Object, Bytes => D.Parameter,
                            Kind => AMC.Kind_Of (D.Authority_Tag)), Grown);
                     end if;
                     Kept := Grown.Status = Window_Store.Grown;
                  end;
               end if;
            end if;
            if not Kept then
               Refused := Refused + 1;
            end if;
         end;
      end loop;
   end Initialize;

   function PCI_Root_Count return Natural is (Window_Store.Length (PCI_Roots));
   function PCI_Root_At (K : Positive) return ACPI_Window is
     (Window_Store.Element_At (PCI_Roots,K));

   function ACPI_Window_Count return Natural is (Window_Store.Length (Windows));
   function ACPI_Window_At (K : Positive) return ACPI_Window is
     (Window_Store.Element_At (Windows, K));
   function ACPI_Windows_Refused return Natural is (Refused);

   function Data_Root_Count return Natural is (Data_Store.Length (Data));
   function Data_Root_At (K : Positive) return Data_Root is (Data_Store.Element_At (Data, K));

   function Device_Root_Count return Natural is (Extent_Store.Length (Extents));
   procedure Device_Root_Extent (K : Positive; First, Bytes : out Unsigned_64) is
      E : constant Extent := Extent_Store.Element_At (Extents, K);
   begin
      First := E.First;
      Bytes := E.Bytes;
   end Device_Root_Extent;

   function Valid return Boolean is (Held.Valid);
   function Roots return BR.Roots is (Held);

   --  The root covering the range, ACPI's pages excluded; nothing claimed.
   function Covering_Root (Physical, Bytes : Unsigned_64) return H.Device_Memory is
   begin
      if Bytes = 0 or else Physical > Unsigned_64'Last - Bytes
        or else not Ledgers.Clear_Of_ACPI (Claims, Physical, Bytes)
      then
         return H.No_Device_Memory;
      end if;
      for R in 1 .. Extent_Store.Length (Extents) loop
         declare
            E : constant Extent := Extent_Store.Element_At (Extents, R);
         begin
            if E.Bytes /= 0
              and then Physical >= E.First
              and then Physical - E.First <= E.Bytes
              and then Bytes <= E.Bytes - (Physical - E.First)
            then
               return E.Memory;
            end if;
         end;
      end loop;
      return H.No_Device_Memory;
   end Covering_Root;

   function Covered (Physical, Bytes : Unsigned_64) return Boolean is
     (H.Valid (Covering_Root (Physical, Bytes)));

   function Device_Region (Physical, Bytes : Unsigned_64) return H.Device_Memory is
      Root : constant H.Device_Memory := Covering_Root (Physical, Bytes);
      Result : MC.Claim_Result;
      Report : Capacity_Report;
   begin
      if not H.Valid (Root) then
         return H.No_Device_Memory;
      end if;
      --  The device's now: no data envelope will overlap it. A ledger that
      --  cannot grow refuses the range (fail closed) and says why.
      Ledgers.Claim (Claims, Physical, Bytes, MC.Device, Result, Report);
      if Result /= MC.Granted then
         if Report.Status /= Ledgers.Port_Store.Grown then
            Device_Failure := Report;
            Device_Failed := True;
         end if;
         return H.No_Device_Memory;
      end if;
      return Root;
   end Device_Region;

   function Device_Copy (Physical, Bytes : Unsigned_64; Kept : H.Rights) return H.Derivation is
     (H.Narrowed (Device_Region (Physical, Bytes), Kept, Physical, Bytes));

   procedure Claim_Data
     (Physical, Bytes : Unsigned_64; Result : out MC.Claim_Result; Report : out Capacity_Report) is
   begin
      Ledgers.Claim (Claims, Physical, Bytes, MC.ACPI_Data, Result, Report);
   end Claim_Data;

   function Clear_Of_Devices (Physical, Bytes : Unsigned_64) return Boolean is
     (Ledgers.Clear_Of_Devices (Claims, Physical, Bytes));

   procedure Claim_Device_Pages
     (First_Page, End_Page : Unsigned_64; Result : out MC.Claim_Result; Report : out Capacity_Report) is
   begin
      Ledgers.Claim (Claims, First_Page * MC.Page_Bytes, (End_Page - First_Page) * MC.Page_Bytes,
                     MC.Device, Result, Report);
   end Claim_Device_Pages;

   function Next_Claim_Start (Page : Unsigned_64) return Unsigned_64 is
     (Ledgers.Next_Start (Claims, Page));

   procedure Take_Capacity_Failure (Report : out Capacity_Report; Failed : out Boolean) is
   begin
      Report := Device_Failure;
      Failed := Device_Failed;
      Device_Failed := False;
   end Take_Capacity_Failure;

end Devmgr_Authority;
