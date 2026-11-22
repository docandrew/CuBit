pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with Firmware_Tables; use Firmware_Tables;
with Firmware_Tables.Snapshots;
with ACPI_Snapshot_Manifest; use ACPI_Snapshot_Manifest;
procedure Manifest_Tests is
   package Snapshots renames Firmware_Tables.Snapshots;
   use type Snapshots.Metadata_Status;
   use type Interfaces.Unsigned_8;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   Default_Limits : constant Quotas := (32, 1_048_576, 65_536);
   Items : Catalog (7 .. 9) :=
     [(1, "DSDT", 2, 36), (2, "SSDT", 2, 40), (3, "TEST", 1, 100)];
   Empty : Catalog (1 .. 0);
   P : Plan;

   function Make (Name : Signature; Length : Positive) return Bytes is
      Data : Bytes (1 .. Length) := [others => 0];
      Sum : Byte := 0;
      Size : Natural := Length;
   begin
      for I in Name'Range loop Data (I) := Character'Pos (Name (I)); end loop;
      for I in 5 .. 8 loop
         Data (I) := Byte (Size mod 256);
         Size := Size / 256;
      end loop;
      Data (9) := 2;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Make;
begin
   Check (not Valid (Empty, 0));
   Check (Analyze (Empty, 0, Default_Limits).Status = Incomplete);
   Check (Analyze (Items, 2, Default_Limits).Status = Incomplete);
   Check (Analyze (Items (7 .. 8), 3, Default_Limits).Status = Incomplete);
   Check (Valid (Items, 3));
   P := Analyze (Items, 3, Default_Limits);
   Check (P.Status = Approved and then P.Needed = (3, 176, 100));
   Check (Analyze (Items, 3, (2, 176, 100)).Status = Table_Quota);
   Check (Analyze (Items, 3, (3, 175, 100)).Status = Byte_Quota);
   Check (Analyze (Items, 3, (3, 176, 99)).Status = Individual_Quota);
   Check (Analyze (Items, 3, (3, 176, 100)).Status = Approved);
   declare
      At_Bound : constant Catalog (Positive'Last - 1 .. Positive'Last) :=
        [(1, "DSDT", 2, 36), (2, "SSDT", 2, 40)];
   begin
      Check (Valid (At_Bound, 2));
      Check (Analyze (At_Bound, 2, Default_Limits).Needed = (2, 76, 40));
   end;
   Items (8).ID := 1;
   Check (Analyze (Items, 3, Default_Limits).Status = Malformed);
   Check (Analyze (Items, 3, Default_Limits).Needed = (0, 0, 0));
   Items (8).ID := 3;
   Check (Analyze (Items, 3, Default_Limits).Status = Malformed);
   Items (8).ID := 2;
   Items (8).Name := "FACS";
   Check (Analyze (Items, 3, Default_Limits).Status = Malformed);
   Items (8).Name := "DSDT";
   Check (Analyze (Items, 3, Default_Limits).Status = Malformed);
   Items (8).Name := "SSDT";
   Items (7).Name := "TEST";
   Check (Analyze (Items, 3, Default_Limits).Status = Malformed);
   Items (7).Name := "DSDT";
   -- Firmware size is measured, not truncated to prototype policy.
   Items (9).Extent := Default_Limits.Bytes + 1;
   P := Analyze (Items, 3, Default_Limits);
   Check (P.Status = Byte_Quota
     and then P.Needed.Bytes = Byte_Count (Default_Limits.Bytes) + 77);
   Check (Analyze (Items, 3, (3, Positive (P.Needed.Bytes), Items (9).Extent)).Status = Approved);
   Items (7).Extent := Positive'Last;
   Items (8).Extent := Positive'Last;
   Items (9).Extent := Positive'Last;
   P := Analyze (Items, 3, (Positive'Last, Positive'Last, Positive'Last));
   Check (P.Status = Byte_Quota
     and then P.Needed.Bytes = 3 * Byte_Count (Positive'Last)
     and then P.Needed.Largest = Positive'Last);
   declare
      Many : Catalog (1 .. Default_Limits.Tables + 1);
   begin
      for I in Many'Range loop
         Many (I) := (Snapshots.Table_ID (I),
           (if I = 1 then "DSDT" else "SSDT"), 2, Table_Header_Size);
      end loop;
      P := Analyze (Many, Many'Length, Default_Limits);
      Check (P.Status = Table_Quota and then P.Needed.Tables = Many'Length);
      Check (Analyze (Many, Many'Length,
        (Many'Length, Default_Limits.Bytes, Default_Limits.Individual)).Status = Approved);
   end;
   -- A real sealed snapshot feeds the same address-free sizing path. Allocate
   -- a packed copy from the measured size rather than the default quotas.
   declare
      First : constant Bytes := Make ("DSDT", Table_Header_Size);
      Large : constant Bytes := Make ("TEST", Default_Limits.Individual + 1);
      Boot : Snapshots.State (2, First'Length + Large'Length, Large'Length);
      Public : Catalog (5 .. 6);
      Copied : Boolean;
   begin
      Snapshots.Begin_Snapshot (Boot, 2);
      Snapshots.Append (Boot,
        (1, First'Length, "DSDT", 2), First, Copied);
      Check (Copied);
      Snapshots.Append (Boot,
        (2, Large'Length, "TEST", 2), Large, Copied);
      Check (Copied);
      Snapshots.Seal (Boot);
      for I in 1 .. 2 loop
         declare
            Metadata : constant Snapshots.Metadata_Result := Snapshots.Describe (Boot, I);
         begin
            Check (Metadata.Status = Snapshots.Metadata_Available);
            Public (I + 4) := Metadata.Info;
         end;
      end loop;
      P := Analyze (Public, Snapshots.Count (Boot),
        (2, First'Length + Large'Length, Large'Length));
      Check (P.Status = Approved);
      declare
         Data : Bytes (1 .. Positive (P.Needed.Bytes));
         Offset : Natural := 0;
      begin
         for Item of Public loop
            Snapshots.Copy (Boot, Positive (Item.ID),
              Data (Offset + 1 .. Offset + Item.Extent), Copied);
            Check (Copied);
            Offset := Offset + Item.Extent;
         end loop;
         Check (Offset = Data'Length);
         Check (Data (1 .. First'Length) = First);
         Check (Data (First'Length + 1 .. Data'Last) = Large);
      end;
   end;
   Ada.Text_IO.Put_Line ("Public manifest checks" & Checks'Image);
end Manifest_Tests;
