------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Devmgr_Claim_Ledgers - implementation
------------------------------------------------------------------------------
pragma Ada_2022;

package body Devmgr_Claim_Ledgers is

   use type PC.Take_Result;
   use type MC.Claim_Result;
   use type Port_Store.Growth_Status;
   use type Memory_Store.Growth_Status;

   --  Memory_Store's report as the one Capacity_Report type.
   function As_Report (R : Memory_Store.Capacity_Report) return Capacity_Report is
     ((Status    => Port_Store.Growth_Status'Val (Memory_Store.Growth_Status'Pos (R.Status)),
       Demand    => R.Demand,
       Limit     => R.Limit,
       Requested => R.Requested));

   procedure Initialize (L : in out Port_Ledger; Report : out Capacity_Report) is
   begin
      Port_Store.Clear (L.Claims);
      Port_Store.Reserve (L.Claims, PC.Seed_Claims, Report);
      if Report.Status /= Port_Store.Grown then
         return;
      end if;
      declare
         Items : PC.Claim_Array (1 .. Port_Store.Capacity (L.Claims))
         with Import, Address => Port_Store.Block (L.Claims);
         Used : Natural;
      begin
         PC.Initialize (Items, Used);
         Port_Store.Set_Length (L.Claims, Used);
      end;
   end Initialize;

   procedure Take
     (L : in out Port_Ledger; First, Ports : Unsigned_64; By : PC.Holder;
      Owner : PC.Owner_Word; Granted : out Boolean; Report : out Capacity_Report)
   is
      Result : PC.Take_Result := PC.Needs_Room;
   begin
      Granted := False;
      Report := (Status => Port_Store.Grown, Demand => 0, Limit => 0, Requested => 0);
      --  At most twice: once more after the array grew.
      for Attempt in 1 .. 2 loop
         declare
            Items : PC.Claim_Array (1 .. Port_Store.Capacity (L.Claims))
            with Import, Address => Port_Store.Block (L.Claims);
            Used : Natural := Port_Store.Length (L.Claims);
         begin
            PC.Take (Items, Used, First, Ports, By, Owner, Result);
            Port_Store.Set_Length (L.Claims, Used);
         end;
         exit when Result /= PC.Needs_Room;
         Port_Store.Reserve (L.Claims, Port_Store.Length (L.Claims) + 1, Report);
         exit when Report.Status /= Port_Store.Grown;
      end loop;
      Granted := Result = PC.Granted;
   end Take;

   procedure Release_Owner (L : in out Port_Ledger; Owner : PC.Owner_Word; Released : out Natural) is
      Items : PC.Claim_Array (1 .. Port_Store.Capacity (L.Claims))
      with Import, Address => Port_Store.Block (L.Claims);
      Before : constant Natural := Port_Store.Length (L.Claims);
      Used : Natural := Before;
   begin
      PC.Release_Owner (Items, Used, Owner);
      Port_Store.Set_Length (L.Claims, Used);
      Released := Before - Used;
   end Release_Owner;

   function Count (L : Port_Ledger) return Natural is (Port_Store.Length (L.Claims));
   function Claim_At (L : Port_Ledger; Index : Positive) return PC.Claim is
     (Port_Store.Element_At (L.Claims, Index));

   procedure Claim
     (L : in out Memory_Ledger; Physical, Bytes : Unsigned_64; By : MC.Holder;
      Result : out MC.Claim_Result; Report : out Capacity_Report)
   is
      Grown : Memory_Store.Capacity_Report;
   begin
      Result := MC.Needs_Room;
      Report := (Status => Port_Store.Grown, Demand => 0, Limit => 0, Requested => 0);
      for Attempt in 1 .. 2 loop
         declare
            Items : MC.Claim_Array (1 .. Memory_Store.Capacity (L.Claims))
            with Import, Address => Memory_Store.Block (L.Claims);
            Used : Natural := Memory_Store.Length (L.Claims);
         begin
            MC.Claim_Pages (Items, Used, Physical, Bytes, By, Result);
            Memory_Store.Set_Length (L.Claims, Used);
         end;
         exit when Result /= MC.Needs_Room;
         Memory_Store.Reserve (L.Claims, Memory_Store.Length (L.Claims) + 1, Grown);
         Report := As_Report (Grown);
         exit when Grown.Status /= Memory_Store.Grown;
      end loop;
   end Claim;

   function Clear_Of_ACPI (L : Memory_Ledger; Physical, Bytes : Unsigned_64) return Boolean is
      Items : constant MC.Claim_Array (1 .. Memory_Store.Capacity (L.Claims))
      with Import, Address => Memory_Store.Block (L.Claims);
   begin
      return MC.Clear_Of_ACPI (Items, Memory_Store.Length (L.Claims), Physical, Bytes);
   end Clear_Of_ACPI;

   function Clear_Of_Devices (L : Memory_Ledger; Physical, Bytes : Unsigned_64) return Boolean is
      Items : constant MC.Claim_Array (1 .. Memory_Store.Capacity (L.Claims))
      with Import, Address => Memory_Store.Block (L.Claims);
   begin
      return MC.Clear_Of_Devices (Items, Memory_Store.Length (L.Claims), Physical, Bytes);
   end Clear_Of_Devices;

   function Count (L : Memory_Ledger) return Natural is (Memory_Store.Length (L.Claims));
   function Claim_At (L : Memory_Ledger; Index : Positive) return MC.Claim is
     (Memory_Store.Element_At (L.Claims, Index));

   function Capacity_Text (What : String; Report : Capacity_Report) return String is
      function Image (Value : Unsigned_64) return String is
         Text : constant String := Unsigned_64'Image (Value);
      begin
         return Text (Text'First + 1 .. Text'Last);
      end Image;
   begin
      return "CAPACITY " & What & ": demand " & Image (Report.Demand) &
        ", limit " & Image (Report.Limit) &
        (case Report.Status is
           when Port_Store.Grown => "",
           when Port_Store.Heap_Refused =>
             ", heap refused " & Image (Report.Requested) & " bytes",
           when Port_Store.Representation_Limit => ", past the representable count");
   end Capacity_Text;

end Devmgr_Claim_Ledgers;
