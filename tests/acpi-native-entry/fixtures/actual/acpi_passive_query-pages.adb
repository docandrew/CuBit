pragma Ada_2022;
package body ACPI_Passive_Query.Pages with SPARK_Mode is
   Bits_Per_Byte : constant := Byte'Size;
   Byte_Mask : constant Word := Word (Byte'Last);
   function Encode (Item : Request; Limit : Node_Limit) return Frame_Bytes is
      Data : constant Words := ACPI_Passive_Query.Encode (Item, Limit);
      Result : Frame_Bytes;
   begin
      for I in Result'Range loop
         Result (I) := Byte (Interfaces.Shift_Right
           (Data (I / Bytes_Per_Word), Bits_Per_Byte * (I mod Bytes_Per_Word)) and Byte_Mask);
      end loop;
      return Result;
   end Encode;
   function Canonical (Data : Byte_Array; Expected : Frame_Bytes) return Boolean is
   begin
      if Data'Length < Frame_Byte_Count then return False; end if;
      for I in Expected'Range loop
         if Data (Data'First + I) /= Expected (I) then return False; end if;
      end loop;
      for I in Data'Range loop
         if I - Data'First >= Frame_Byte_Count and then Data (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Canonical;
   function Required_Bytes (Item : Request) return Word is
     (Frame_Byte_Count + (if Is_External (Item.Key.ID) then Word (Text_Length (Item.Key.ID)) *
       (if Item.Continuation.Present then 2 else 1) else 0));
   function Valid_Request_Layout (Item : Request) return Boolean is
     (not Is_External (Item.Key.ID) or else
       (Text_Offset (Item.Key.ID) = Frame_Byte_Count and then
        (if Item.Continuation.Present then
          Is_External (Item.Continuation.Value.Filter.ID) and then
          Word (Text_Offset (Item.Continuation.Value.Filter.ID)) =
            Frame_Byte_Count + Word (Text_Length (Item.Key.ID)))));
   function Span_Fits (Item : Device_ID; Data : Byte_Array) return Boolean is
      Offset, Length : Word;
   begin
      if not Is_External (Item) then return False; end if;
      Offset := Word (Text_Offset (Item)); Length := Word (Text_Length (Item));
      if Data'Length < Frame_Byte_Count or else Length = 0
        or else Offset < Frame_Byte_Count or else Offset > Word (Natural'Last)
        or else Length > Word (Natural'Last)
        or else Offset > Word (Data'Length) or else Length > Word (Data'Length) - Offset
      then return False; end if;
      return Natural (Offset) <= Data'Last - Data'First and then
        Natural (Length) - 1 <= Data'Last - Data'First - Natural (Offset);
   end Span_Fits;
   function Same_ID_Value
     (Left : Device_ID; Left_Data : Byte_Array; Right : Device_ID; Right_Data : Byte_Array)
      return Boolean is
   begin
      if not Is_External (Left) or else not Is_External (Right) then
         return Valid_ID (Left) and then Valid_ID (Right) and then Left = Right;
      end if;
      if not Span_Fits (Left, Left_Data) or else not Span_Fits (Right, Right_Data)
        or else Text_Length (Left) /= Text_Length (Right) then return False; end if;
      for I in 0 .. Natural (Text_Length (Left)) - 1 loop
         declare L : constant Byte := Left_Data (Left_Data'First + (Natural (Text_Offset (Left)) + I)); begin
            if L = 0 or else L /= Right_Data (Right_Data'First + (Natural (Text_Offset (Right)) + I))
            then return False; end if;
         end;
      end loop;
      return True;
   end Same_ID_Value;
   function ID_Text_Valid (Item : Request; Data : Byte_Array) return Boolean is
   begin
      if not Is_External (Item.Key.ID) then return Valid_ID (Item.Key.ID) or else Item.Key.Kind /= Find_By_ID; end if;
      if not Same_ID_Value (Item.Key.ID, Data, Item.Key.ID, Data) then return False; end if;
      return not Item.Continuation.Present or else
        Same_ID_Value (Item.Key.ID, Data, Item.Continuation.Value.Filter.ID, Data);
   end ID_Text_Valid;
   procedure Rebase_Text
     (Item : in out Request; Origin : Interfaces.Unsigned_32; Limit : Node_Limit; Accepted : out Boolean) is
      Length : Interfaces.Unsigned_32;
   begin
      Accepted := False;
      if not Valid (Item, Limit) then return; end if;
      if Is_External (Item.Key.ID) then
         Length := Text_Length (Item.Key.ID);
         if Item.Continuation.Present and then Word (Origin) + Word (Length) > Word (Interfaces.Unsigned_32'Last)
         then return; end if;
         Item.Key.ID := External_ID (Origin, Length);
         if Item.Continuation.Present then
            Item.Continuation.Value.Filter.ID := External_ID
              (Interfaces.Unsigned_32 (Word (Origin) + Word (Length)), Length);
         end if;
      end if;
      Accepted := True;
   end Rebase_Text;
   procedure Encode_Request
     (Item : Request; Text : Byte_Array; Data : out Byte_Array; Limit : Node_Limit;
      Written : out Word; Accepted : out Boolean) is
   begin
      Data := [others => 0]; Written := 0; Accepted := False;
      if not Valid (Item, Limit) or else not Valid_Request_Layout (Item)
        or else Required_Bytes (Item) > Word (Data'Length)
        or else Text'Length > Natural'Last
        or else (if Is_External (Item.Key.ID) then Word (Text'Length) /= Word (Text_Length (Item.Key.ID))
                 else Text'Length /= 0)
      then return; end if;
      declare Header : constant Frame_Bytes := Encode (Item, Limit); begin
         for I in Data'Range loop
            declare Offset : constant Natural := I - Data'First; begin
               if Offset < Frame_Byte_Count then Data (I) := Header (Offset);
               elsif Word (Offset) < Required_Bytes (Item) then
                  if Text'Length = 0 then Data := [others => 0]; return; end if;
                  declare Source : constant Natural := (Offset - Frame_Byte_Count) mod Text'Length; begin
                     Data (I) := Text (Text'First + Source);
                  end;
               end if;
            end;
         end loop;
      end;
      if not ID_Text_Valid (Item, Data) then Data := [others => 0]; return; end if;
      Written := Required_Bytes (Item); Accepted := True;
   end Encode_Request;
   function Decode (Data : Byte_Array; Limit : Node_Limit) return Decode_Result is
      Frame : Words := [others => 0];
   begin
      if Data'Length < Frame_Byte_Count then return (Status => Malformed); end if;
      for I in Frame_Bytes'Range loop
         Frame (I / Bytes_Per_Word) := Frame (I / Bytes_Per_Word) or
           Interfaces.Shift_Left (Word (Data (Data'First + I)),
             Bits_Per_Byte * (I mod Bytes_Per_Word));
      end loop;
      declare Result : constant Decode_Result := ACPI_Passive_Query.Decode (Frame, Limit); begin
         if Result.Status /= Decoded then return Result; end if;
         if not Valid_Request_Layout (Result.Value) or else Required_Bytes (Result.Value) > Word (Data'Length)
           or else not ID_Text_Valid (Result.Value, Data) then return (Status => Malformed); end if;
         for I in Data'Range loop
            if Word (I - Data'First) >= Required_Bytes (Result.Value) and then Data (I) /= 0
            then return (Status => Malformed); end if;
         end loop;
         if Is_External (Result.Value.Key.ID) or else Canonical (Data, Encode (Result.Value, Limit)) then
            return Result;
         end if;
         return (Status => Malformed);
      end;
   end Decode;
end ACPI_Passive_Query.Pages;
