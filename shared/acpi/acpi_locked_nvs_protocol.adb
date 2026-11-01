pragma Ada_2022;
with ACPI_Inventory_Protocol;
package body ACPI_Locked_NVS_Protocol with SPARK_Mode is
   function Valid (Item : Geometry) return Boolean is
     (Item.Offset <= Maximum_Offset and then Item.Offset mod Unsigned_64 (T.Octets (Item.Width)) = 0);
   function Pack (Item : Geometry) return Unsigned_64 is
     (Item.Offset * Width_Count + T.Access_Width'Pos (Item.Width));
   function Unpack (Item : Unsigned_64) return Geometry is
     ((Item / Width_Count, T.Access_Width'Val (Item mod Width_Count)));
   function Label_For (Kind : Operation; Response : Boolean := False) return Unsigned_32 is
     ((if Response then First_Reply else First_Operation) + Operation'Pos (Kind));
   function Owned_Label (Label : Unsigned_32) return Boolean is
     (Label in First_Operation .. Label_For (Operation'Last));
   function Framed (Item : Packet) return Boolean is
     (Item.Length = 4 and then Item.Flags = 0 and then Item.Reserved = 0);
   function Select_Request (Lease : Lease_Ticket; ID : Record_ID; Datum : Geometry) return Packet is
     ((Label => Label_For (Select_Datum), Data => [Lease.Domain, Lease.Sequence, Unsigned_64 (ID), Pack (Datum)], others => <>));
   function Read_Request (Kind : Operation; Lease : Lease_Ticket;
     Descriptor : Descriptor_ID; Sequence : Read_Sequence) return Packet is
     ((Label => Label_For (Kind), Data => [Lease.Domain, Lease.Sequence, Unsigned_64 (Descriptor), Unsigned_64 (Sequence)], others => <>));
   function Decode_Request (Item : Packet) return Request is
      Result : Request;
   begin
      if not Framed (Item) or else not Owned_Label (Item.Label) then return Result; end if;
      Result.Kind := Operation'Val (Item.Label - First_Operation);
      Result.Lease := (Item.Data (0), Item.Data (1));
      if not ACPI_Global_Lock_Protocol.Valid (Result.Lease) then return (others => <>); end if;
      if Result.Kind = Select_Datum then
         Result.ID := Record_ID (Item.Data (2)); Result.Datum := Unpack (Item.Data (3));
         if Result.ID = No_Record or else not Valid (Result.Datum) then return (others => <>); end if;
      else
         Result.Descriptor := Descriptor_ID (Item.Data (2)); Result.Sequence := Read_Sequence (Item.Data (3));
         if Result.Descriptor = No_Descriptor or else Result.Sequence = No_Read then return (others => <>); end if;
      end if;
      Result.Valid := True; return Result;
   end Decode_Request;
   function Valid_Result (Kind : Operation; Item : Reply) return Boolean is
   begin
      if not Item.Valid then return False; end if;
      if Item.Kind = Prepared then
         return Kind = Select_Datum and then Item.Descriptor /= No_Descriptor and then
           Item.ID /= No_Record and then Valid (Item.Datum) and then
           Item.Sequence = No_Read and then Item.Outcome = No_Data and then Item.Value = 0;
      elsif Item.Kind in Recorded | Cached then
         return Kind /= Select_Datum and then (Kind /= Query_Read or else Item.Kind = Cached)
           and then Item.Descriptor /= No_Descriptor and then Item.Sequence /= No_Read
           and then Item.ID = No_Record and then Item.Datum = Empty_Geometry
           and then Item.Outcome /= No_Data and then (Item.Outcome = Done or else Item.Value = 0);
      else
         return Item.Descriptor = No_Descriptor and then Item.Sequence = No_Read
           and then Item.ID = No_Record and then Item.Datum = Empty_Geometry
           and then Item.Outcome = No_Data and then Item.Value = 0;
      end if;
   end Valid_Result;
   function Encode_Reply (Kind : Operation; Item : Reply) return Packet is
     ((Label => Label_For (Kind, True),
       Data => [Reply_Kind'Pos (Item.Kind) * Outcome_Base + Read_Outcome'Pos (Item.Outcome),
                Unsigned_64 (Item.Descriptor),
                (if Item.Kind = Prepared then Unsigned_64 (Item.ID) else Unsigned_64 (Item.Sequence)),
                (if Item.Kind = Prepared then Pack (Item.Datum) else Item.Value)], others => <>));
   function Decode_Reply (Kind : Operation; Item : Packet; Width : T.Access_Width := T.Byte_Access) return Reply is
      Result : Reply;
   begin
      if not Framed (Item) or else Item.Label /= Label_For (Kind, True)
        or else Item.Data (0) > Maximum_Status
        or else Item.Data (0) mod Outcome_Base > Read_Outcome'Pos (Read_Outcome'Last) then return Result; end if;
      Result.Kind := Reply_Kind'Val (Item.Data (0) / Outcome_Base);
      Result.Outcome := Read_Outcome'Val (Item.Data (0) mod Outcome_Base);
      Result.Descriptor := Descriptor_ID (Item.Data (1));
      if Result.Kind = Prepared then
         Result.ID := Record_ID (Item.Data (2)); Result.Datum := Unpack (Item.Data (3));
      else
         Result.Sequence := Read_Sequence (Item.Data (2)); Result.Value := Item.Data (3);
      end if;
      Result.Valid := True;
      if not Valid_Result (Kind, Result) or else
        (Result.Outcome = Done and then Result.Value > ACPI_Inventory_Protocol.Width_Mask (Width))
      then return (others => <>); end if;
      return Result;
   end Decode_Reply;
   function Matches_Selection (Item : Reply; ID : Record_ID; Datum : Geometry) return Boolean is
     (Valid_Result (Select_Datum, Item) and then Item.Kind = Prepared and then Item.ID = ID and then Item.Datum = Datum);
   function Matches_Read (Item : Reply; Descriptor : Descriptor_ID; Sequence : Read_Sequence) return Boolean is
     (Valid_Result (Read_Datum, Item) and then Item.Kind in Recorded | Cached
       and then Item.Descriptor = Descriptor and then Item.Sequence = Sequence);
end ACPI_Locked_NVS_Protocol;
