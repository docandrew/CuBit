pragma Ada_2022;
package body ACPI_Query_Transport with SPARK_Mode is
   function Decode_Request (Data : Packet) return Request_Decode is
      Result : Request_Decode := (Selected => Data.Label = Request_Label, others => <>);
   begin
      if not Canonical_Header (Data)
        or else Data.Data (Request_Reserved_First .. Last_Index) /= [0, 0]
      then return Result; end if;
      Result.Decoded := True;
      Result.Reference_Word := Data.Data (Reference_Index);
      Result.Declared_Length := Data.Data (Length_Index);
      return Result;
   end Decode_Request;
   function Encode_Request (Reference_Word, Declared_Length : Word) return Packet is
     (Label => Request_Label, Length => Canonical_Length, Flags => 0, Reserved => 0,
      Data => [Reference_Word, Declared_Length, 0, 0]);
   function Status_From_Code (Value : Word) return Call_Status is
     (Call_Status'Val (Natural (Value)));
   function Decode_Reply (Data : Packet; Declared_Length : Word) return Reply_Decode is
      Result : Reply_Decode := (Selected => Data.Label = Reply_Label, others => <>);
   begin
      if not Canonical_Header (Data) or else not Valid_Reply_Words (Data.Data, Declared_Length)
      then return Result; end if;
      Result.Decoded := True;
      Result.Value := (Status_From_Code (Data.Data (Status_Index)),
        Data.Data (Written_Index), Data.Data (Required_Index));
      return Result;
   end Decode_Reply;
   function Encode_Reply (Value : Reply_Details) return Packet is
      Selected : constant Reply_Details :=
        (if Valid (Value) then Value else (Status => Invalid_Response, Written => 0, Required => 0));
   begin
      return (Label => Reply_Label, Length => Canonical_Length, Flags => 0, Reserved => 0,
        Data => [Status_Code (Selected.Status), Selected.Written, Selected.Required, 0]);
   end Encode_Reply;
end ACPI_Query_Transport;
