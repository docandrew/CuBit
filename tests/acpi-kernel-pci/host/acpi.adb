with Interfaces;
with Firmware_Tables.MCFG;
with Physical_Ranges;
package body ACPI is
   package FT renames Firmware_Tables;
   use type FT.Byte;
   use type Interfaces.Unsigned_64;
   Storage : FT.Bytes
     (1 .. FT.MCFG.Fixed_Size +
       (Physical_Ranges.Max_PCI_Allocations + 1) * FT.MCFG.Allocation_Size);
   procedure Put_LE (Offset, Width : Positive; Value : Interfaces.Unsigned_64) is
      Remaining : Interfaces.Unsigned_64 := Value;
   begin
      for I in Offset .. Offset + Width - 1 loop
         Storage (I) := FT.Byte (Remaining mod 256);
         Remaining := Remaining / 256;
      end loop;
   end Put_LE;
   procedure Configure (Allocations : Natural) is
      Sum : FT.Byte := 0;
   begin
      Snapshot_Mode := FT.Snapshots.Ready;
      Descriptors := 1;
      Metadata_Missing := False;
      Copy_Failure := False;
      Corrupt_Data := False;
      First_Name := "MCFG";
      Advertised_Bytes := FT.MCFG.Fixed_Size + Allocations * FT.MCFG.Allocation_Size;
      Copy_Calls := 0; Describe_Calls := 0;
      Copy_First := 0; Copy_Length := 0; Copy_Index := 0;
      Storage := [others => 0];
      Storage (1 .. 4) := [Character'Pos ('M'), Character'Pos ('C'),
                          Character'Pos ('F'), Character'Pos ('G')];
      Storage (9) := 1;
      Put_LE (5, 4, Interfaces.Unsigned_64 (Advertised_Bytes));
      for I in 1 .. Allocations loop
         declare Offset : constant Positive := FT.MCFG.Fixed_Size + 1 +
           (I - 1) * FT.MCFG.Allocation_Size;
         begin
            Put_LE (Offset, 8, 16#1_0000_0000# +
                      Interfaces.Unsigned_64 (I) * 2 * Physical_Ranges.PCI_Bus_Bytes);
            Put_LE (Offset + 8, 2, Interfaces.Unsigned_64 (I));
         end;
      end loop;
      for B of Storage (1 .. Advertised_Bytes) loop Sum := Sum + B; end loop;
      Storage (10) := 0 - Sum;
   end Configure;
   function Describe_Snapshot_Table (Index : Natural)
     return FT.Snapshots.Metadata_Result is
   begin
      Describe_Calls := Describe_Calls + 1;
      if Metadata_Missing then return (Status => FT.Snapshots.Unknown_Table); end if;
      return (Status => FT.Snapshots.Metadata_Available,
              Info => (ID => FT.Snapshots.Table_ID (Index),
                       Name => (if Index = 1 then First_Name else "MCFG"),
                       Revision => 1, Extent => Advertised_Bytes));
   end Describe_Snapshot_Table;
   procedure Copy_Table (Index : Positive; Destination : out FT.Bytes;
                         Result : out Table_Copy_Status) is
   begin
      Copy_Calls := Copy_Calls + 1;
      Copy_First := Destination'First;
      Copy_Length := Destination'Length;
      Copy_Index := Index;
      Destination := [others => 0];
      if Copy_Failure then Result := Snapshot_Unavailable;
      elsif Corrupt_Data then Result := Copied;
      elsif Destination'Length /= Advertised_Bytes then Result := Buffer_Too_Small;
      else
         Destination := Storage (1 .. Advertised_Bytes);
         Result := Copied;
      end if;
   end Copy_Table;
   function Table_Source (Index : Positive) return FT.Catalog.Descriptor is
      pragma Unreferenced (Index);
   begin
      raise Program_Error with "no hosted immutable table extents";
      return (others => <>);
   end Table_Source;
end ACPI;
