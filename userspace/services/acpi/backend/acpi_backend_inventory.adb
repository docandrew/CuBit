pragma Ada_2022;
package body ACPI_Backend_Inventory is
   package Proto renames ACPI_Inventory_Protocol;
   package Wire renames Proto.Wire;
   use Interfaces;
   use type Catalog.P.Address_Space, Wire.Words;
   procedure Dispatch
     (State : Catalog.Context; Stamp : Unsigned_64;
      Item : Wire.Packet; Reply : out Wire.Packet)
   is
      Info : constant Catalog.Inventory := Catalog.Header (State, Stamp);
   begin
      Reply := (Label => (if Item.Label = Proto.Header_Operation then Proto.Header_Reply else Proto.Part_Reply),
                Data => [Proto.Outcome'Pos (Proto.Malformed), 0, 0, 0], others => <>);
      if not Proto.Envelope_Valid (Item) then return; end if;
      if Item.Label = Proto.Header_Operation then
         if Item.Data /= Wire.Words'[0, 0, 0, 0] then return; end if;
      elsif Item.Label = Proto.Part_Operation then
         if Item.Data (0) = 0 or else Item.Data (1) = 0
           or else Item.Data (2) > Proto.Part_Kind'Pos (Proto.Part_Kind'Last)
           or else Item.Data (3) /= 0 then return; end if;
      else return; end if;
      Reply.Data (0) := Proto.Outcome'Pos (Proto.Denied);
      if not Info.Available then return; end if;
      if Item.Label = Proto.Header_Operation then
         Reply.Data := [Proto.Outcome'Pos (Proto.Ready), Unsigned_64 (Info.Revision),
                        Unsigned_64 (Info.Count), Proto.Version];
         return;
      end if;
      if Item.Data (0) /= Unsigned_64 (Info.Revision) then
         Reply.Data (0) := Proto.Outcome'Pos (Proto.Stale); return;
      end if;
      -- Bound before narrowing. Ordinal zero was refused above; Count is the
      -- scope-filtered bounded count, not the total across other scopes.
      if Item.Data (1) > Unsigned_64 (Info.Count) then return; end if;
      declare
         Description : constant Catalog.Description := Catalog.Describe
           (State, Stamp, Info.Revision, Natural (Item.Data (1)));
         Value : Proto.Description;
      begin
         if not Description.Available then return; end if;
         Value :=
           (ID => Description.Item.Register_Authority.Resource_ID,
            Epoch => Description.Epoch, Base => Description.Item.Base,
            Length => Description.Item.Length, Offset => Description.Item.Register_Offset,
            IO_Space => Description.Item.Space = Catalog.P.IO_Space,
            Readable => Description.Item.Register_Authority.Readable,
            Writable => Description.Item.Register_Authority.Writable,
            Delegable => Description.Item.Register_Authority.Delegable,
            Exact_Write_Safe => Description.Exact_Write_Safe,
            Widths => Proto.Width_Set (Description.Item.Widths),
            Width => Description.Item.Register_Width,
            Write_Mask => Description.Item.Register_Authority.Write_Mask);
         if not Proto.Valid (Value) then return; end if;
         declare
            Data : constant Proto.Parts := Proto.Encode (Value);
            Part : constant Proto.Payload := Data (Proto.Part_Kind'Val (Item.Data (2)));
         begin Reply.Data := [Proto.Outcome'Pos (Proto.Ready), Part (0), Part (1), Part (2)]; end;
      end;
   end Dispatch;
end ACPI_Backend_Inventory;
