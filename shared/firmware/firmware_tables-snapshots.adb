pragma Ada_2022;
with Firmware_Tables.Copies;
package body Firmware_Tables.Snapshots with SPARK_Mode is
   function Contents (S : State) return Image is
     ((S.Table_Capacity, S.Byte_Capacity,
       S.Mode, S.Required, S.Used, S.Total, S.Tables, S.Data));
   function Current (S : State) return Phase is (S.Mode);
   function Count (S : State) return Natural is
     (if S.Mode = Ready then S.Used else 0);
   function Item (S : State; Index : Positive) return Catalog.Descriptor is
     (S.Tables (Index).Description);
   function Value (S : State; Index : Positive; Offset : Natural) return Byte is
     (S.Data (S.Tables (Index).Offset + Offset + 1));
   procedure Begin_Snapshot (S : in out State; Tables : Natural) is
   begin
      if S.Mode /= Empty then return; end if;
      if Tables not in 1 .. S.Table_Capacity then S.Mode := Failed; return; end if;
      S.Used := 0;
      S.Total := 0;
      S.Required := Tables;
      S.Mode := Building;
   end Begin_Snapshot;
   procedure Append
     (S : in out State; Expected : Catalog.Descriptor; Source : Bytes;
      Success : out Boolean) is
   begin
      Success := False;
      if S.Mode /= Building then return; end if;
      if S.Used >= S.Required
        or else Expected.Extent > S.Table_Byte_Limit
        or else Expected.Extent > S.Byte_Capacity - S.Total
        or else (S.Used = 0 and then Expected.Name /= "DSDT")
        or else (S.Used /= 0 and then Expected.Name = "DSDT")
      then S.Mode := Failed; return; end if;
      Copies.Copy_Validated
        (Expected, Source, S.Data (S.Total + 1 .. S.Total + Expected.Extent), Success);
      if not Success then S.Mode := Failed; return; end if;
      S.Tables (S.Used + 1).Description := Expected;
      S.Tables (S.Used + 1).Offset := S.Total;
      S.Used := S.Used + 1;
      S.Total := S.Total + Expected.Extent;
   end Append;
   procedure Seal (S : in out State) is
   begin
      if S.Mode = Building then
         S.Mode := (if S.Used = S.Required then Ready else Failed);
      end if;
   end Seal;
   procedure Reject (S : in out State) is
   begin
      if S.Mode = Building then S.Mode := Failed; end if;
   end Reject;
   procedure Copy
     (S : State; Index : Positive; Destination : out Bytes;
      Success : out Boolean) is
   begin
      Destination := [others => 0];
      Success := False;
      if Index > Count (S) then return; end if;
      declare
         Extent : constant Positive := S.Tables (Index).Description.Extent;
      begin
         if Destination'Length < Extent then return; end if;
         Destination (Destination'First .. Destination'First + (Extent - 1)) :=
           S.Data (S.Tables (Index).Offset + 1 .. S.Tables (Index).Offset + Extent);
         Success := True;
      end;
   end Copy;
end Firmware_Tables.Snapshots;
