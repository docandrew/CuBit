pragma Ada_2022;
with ACPI_Inventory_Protocol;
package body ACPI_NVS_Protocol with SPARK_Mode is
   use type Wire.Words;
   function Framed (Item : Packet) return Boolean is
     (Item.Length = 4 and then Item.Flags = 0 and then Item.Reserved = 0);
   function Label_For (Kind : Operation) return Unsigned_32 is
     (case Kind is when Header => Header_Reply, when Describe => Describe_Reply, when Read => Read_Reply);
   function Decode_Request (Item : Packet) return Request is
      Result : Request;
   begin
      if not Framed (Item) then return Result; end if;
      case Item.Label is
         when Header_Operation =>
            if Item.Data /= Wire.Words'[others => 0] then return Result; end if;
            Result.Kind := Header;
         when Describe_Operation =>
            if Item.Data (0) = 0 or else Item.Data (1) = 0 or else Item.Data (2) /= 0 or else Item.Data (3) /= 0
            then return Result; end if;
            Result.Kind := Describe; Result.Epoch := Item.Data (0); Result.Position := Item.Data (1);
         when Read_Operation =>
            if Item.Data (0) = 0 or else Item.Data (1) = 0 then return Result; end if;
            case Item.Data (3) is
               when 1 => Result.Width := T.Byte_Access;
               when 2 => Result.Width := T.Word_Access;
               when 4 => Result.Width := T.Dword_Access;
               when 8 => Result.Width := T.Qword_Access;
               when others => return Result;
            end case;
            if Item.Data (2) mod Item.Data (3) /= 0 then return Result; end if;
            Result.Kind := Read; Result.Epoch := Item.Data (0); Result.ID := Item.Data (1);
            Result.Position := Item.Data (2);
         when others => return Result;
      end case;
      Result.Valid := True; return Result;
   end Decode_Request;
   function Header_Request return Packet is ((Label => Header_Operation, others => <>));
   function Describe_Request (Epoch, Ordinal : Unsigned_64) return Packet is
     ((Label => Describe_Operation, Data => [Epoch, Ordinal, 0, 0], others => <>));
   function Read_Request
     (Epoch, ID, Offset : Unsigned_64; Width : T.Access_Width) return Packet is
     ((Label => Read_Operation, Data => [Epoch, ID, Offset, Unsigned_64 (T.Octets (Width))], others => <>));
   function Error_Reply (Kind : Operation; Status : Outcome) return Packet is
     ((Label => Label_For (Kind), Data => [Outcome'Pos (Status), 0, 0, 0], others => <>));
   function Header_Result (Epoch, Count : Unsigned_64) return Packet is
     ((Label => Header_Reply, Data => [Outcome'Pos (Ready), Version, Epoch, Count], others => <>));
   function Describe_Result (ID, Base, Length : Unsigned_64) return Packet is
     ((Label => Describe_Reply, Data => [Outcome'Pos (Ready), ID, Base, Length], others => <>));
   function Read_Result (Value : Unsigned_64) return Packet is
     ((Label => Read_Reply, Data => [Outcome'Pos (Done), Value, 0, 0], others => <>));
   function Decode_Reply
     (Item : Packet; Kind : Operation; Width : T.Access_Width := T.Byte_Access) return Reply is
      Result : Reply;
      Status : Outcome;
   begin
      if not Framed (Item) or else Item.Label /= Label_For (Kind)
        or else Item.Data (0) > Outcome'Pos (Outcome'Last) then return Result; end if;
      Status := Outcome'Val (Item.Data (0));
      if Status not in Ready | Done then
         if Item.Data (1) /= 0 or else Item.Data (2) /= 0 or else Item.Data (3) /= 0 then return Result; end if;
      elsif Kind = Header then
         if Status /= Ready or else Item.Data (1) /= Version or else Item.Data (2) = 0
           or else Item.Data (3) > Maximum_Records then return Result; end if;
         Result.Epoch := Item.Data (2); Result.Count := Item.Data (3);
      elsif Kind = Describe then
         if Status /= Ready or else Item.Data (1) = 0 or else Item.Data (3) = 0
           or else Item.Data (3) - 1 > Unsigned_64'Last - Item.Data (2) then return Result; end if;
         Result.ID := Item.Data (1); Result.Base := Item.Data (2); Result.Length := Item.Data (3);
      else
         if Status /= Done or else Item.Data (2) /= 0 or else Item.Data (3) /= 0
           or else Item.Data (1) > ACPI_Inventory_Protocol.Width_Mask (Width) then return Result; end if;
         Result.Value := Item.Data (1);
      end if;
      Result.Valid := True; Result.Status := Status; return Result;
   end Decode_Reply;
end ACPI_NVS_Protocol;
