pragma Ada_2022;
package body ACPI_Passive_Query.Replies with SPARK_Mode is
   use type Resources_IO.Controller_Kind;
   function Valid_Filter_Layout (Item : Header) return Boolean is
     (if not Is_External (Item.Context.Key.ID) then Item.Filter_Bytes = 0
      else Word (Text_Offset (Item.Context.Key.ID)) = Header_Byte_Count + Word (Item.Payload_Bytes)
        and then Word (Item.Filter_Bytes) = Word (Text_Length (Item.Context.Key.ID)) *
          (if Item.Context.Continuation.Present then 2 else 1)
        and then (if Item.Context.Continuation.Present then
          Is_External (Item.Context.Continuation.Value.Filter.ID) and then
          Word (Text_Offset (Item.Context.Continuation.Value.Filter.ID)) =
            Header_Byte_Count + Word (Item.Payload_Bytes) + Word (Text_Length (Item.Context.Key.ID))));
   function Valid (Item : Header; Limit : Node_Limit) return Boolean is
     (ACPI_Passive_Query.Valid (Item.Context, Limit)
      and then Valid_Filter_Layout (Item)
      and then (if Item.Status = Insufficient_Space then Item.Required_Bytes > Extent (Item)
                else Item.Required_Bytes = 0)
      and then (if Item.Status /= OK then
        Item.Record_Count = 0 and then Item.Payload_Bytes = 0
        and then not Item.Context.Continuation.Present and then not Item.CIDs_Truncated
       else Item.Context.Key.Instance /= 0 and then Item.Context.Key.Revision /= 0
        and then (not Item.CIDs_Truncated or else Item.Context.Key.Kind = Describe)
        and then (case Item.Context.Key.Kind is
          when Find_By_ID | Find_By_Address =>
            Word (Item.Payload_Bytes) = Word (Item.Record_Count) * Node_Byte_Count
            and then Word (Item.Record_Count) <= Word (Limit),
          when Resources => Word (Item.Record_Count) * Resource_Byte_Count <= Word (Item.Payload_Bytes)
            and then (if Item.Record_Count = 0 then Item.Payload_Bytes = 0),
          when Describe => Item.Record_Count <= Item.Payload_Bytes
            and then (if Item.Record_Count = 0 then Item.Payload_Bytes = 0))));
   function Matching_Metadata (Item : Header; Expected : Request; Limit : Node_Limit)
     return Boolean is
     (Valid (Item, Limit) and then ACPI_Passive_Query.Valid (Expected, Limit)
      and then Item.Context.Key.Kind = Expected.Key.Kind
      and then Item.Context.Key.Node = Expected.Key.Node
      and then Item.Context.Key.Address = Expected.Key.Address
      and then (if Is_External (Expected.Key.ID) then Is_External (Item.Context.Key.ID)
        and then Text_Length (Item.Context.Key.ID) = Text_Length (Expected.Key.ID)
        else Item.Context.Key.ID = Expected.Key.ID)
      and then (if Expected.Key.Instance /= 0 then
        Item.Context.Key.Instance = Expected.Key.Instance
        and then Item.Context.Key.Revision = Expected.Key.Revision));
   function Matches_Request (Item : Header; Expected : Request; Limit : Node_Limit)
     return Boolean is
     (not Is_External (Expected.Key.ID) and then Matching_Metadata (Item, Expected, Limit));
   function Matches_Request
     (Data, Request_Data : Bytes_IO.Byte_Array; Limit : Node_Limit) return Boolean is
      Response : constant Header_Result := Read_Envelope (Data, Limit);
      Query : constant Decode_Result := Bytes_IO.Decode (Request_Data, Limit);
   begin
      if not Response.Accepted or else Query.Status /= Decoded
        or else not Matching_Metadata (Response.Value, Query.Value, Limit) then return False; end if;
      return Query.Value.Key.Kind /= Find_By_ID or else Bytes_IO.Same_ID_Value
        (Response.Value.Context.Key.ID, Data, Query.Value.Key.ID, Request_Data);
   end Matches_Request;
   function Encode_Header (Item : Header; Limit : Node_Limit) return Header_Words is
      Data : Header_Words := [others => 0];
      Context : constant Words := ACPI_Passive_Query.Encode (Item.Context, Limit);
   begin
      Data (0) := Header_Magic; Data (1) := Header_Version;
      Data (2) := Query_Status'Pos (Item.Status); Data (3) := Word (Item.Payload_Bytes);
      Data (4) := Word (Item.Record_Count); Data (5) := (if Item.CIDs_Truncated then 1 else 0);
      Data (6) := Item.Required_Bytes;
      Data (31) := Word (Item.Filter_Bytes);
      for I in Context'Range loop Data (7 + I) := Context (I); end loop;
      return Data;
   end Encode_Header;
   function Decode_Header (Data : Header_Words; Limit : Node_Limit) return Header_Result is
      Context : constant Decode_Result := Decode (Words (Data (7 .. 30)), Limit);
      Item : Header;
   begin
      if Data (0) /= Header_Magic or else Data (1) /= Header_Version
        or else Data (2) > Query_Status'Pos (Query_Status'Last)
        or else Data (3) > Word (U32'Last) or else Data (4) > Word (U32'Last)
        or else Data (5) > 1 or else Data (31) > Word (U32'Last) or else Context.Status /= Decoded
      then return (Accepted => False); end if;
      Item := (Status => Query_Status'Val (Natural (Data (2))), Context => Context.Value,
        Payload_Bytes => U32 (Data (3)), Record_Count => U32 (Data (4)),
        Filter_Bytes => U32 (Data (31)), CIDs_Truncated => Data (5) = 1, Required_Bytes => Data (6));
      if Valid (Item, Limit) and then Encode_Header (Item, Limit) = Data then return (True, Item); end if;
      return (Accepted => False);
   end Decode_Header;
   type Word_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Word;
         when False => null;
      end case;
   end record;
   function Read_Word (Data : Bytes_IO.Byte_Array; Offset : Word) return Word_Result is
      Value : Word := 0;
   begin
      if Data'Length < Bytes_IO.Bytes_Per_Word
        or else Offset > Word (Data'Length) or else Word (Data'Length) - Offset < Bytes_IO.Bytes_Per_Word
        or else Offset > Word (Natural'Last) - (Bytes_IO.Bytes_Per_Word - 1)
      then return (Accepted => False); end if;
      if Natural (Offset) > Data'Last - Data'First - (Bytes_IO.Bytes_Per_Word - 1)
      then return (Accepted => False); end if;
      for I in 0 .. Bytes_IO.Bytes_Per_Word - 1 loop
         Value := Value or Interfaces.Shift_Left
           (Word (Data (Data'First + (Natural (Offset) + I))), Bytes_IO.Byte'Size * I);
      end loop;
      return (True, Value);
   end Read_Word;
   procedure Encode_Envelope
     (Item : Header; Payload : Bytes_IO.Byte_Array; Data : out Bytes_IO.Byte_Array;
      Limit : Node_Limit; Written : out Word; Accepted : out Boolean;
      Filter : Bytes_IO.Byte_Array := []) is
   begin
      Data := [others => 0]; Written := 0; Accepted := False;
      if not Valid (Item, Limit) or else Word (Payload'Length) /= Word (Item.Payload_Bytes)
        or else Word (Filter'Length) /= Word (Item.Filter_Bytes)
        or else Extent (Item) > Word (Data'Length) then return; end if;
      declare Words_Out : constant Header_Words := Encode_Header (Item, Limit); begin
         for I in Data'Range loop
            declare Offset : constant Natural := I - Data'First; begin
               if Offset < Header_Byte_Count then
                  Data (I) := Bytes_IO.Byte (Interfaces.Shift_Right
                    (Words_Out (Offset / Bytes_IO.Bytes_Per_Word),
                     Bytes_IO.Byte'Size * (Offset mod Bytes_IO.Bytes_Per_Word)) and 16#FF#);
               elsif Word (Offset) < Header_Byte_Count + Word (Item.Payload_Bytes) then
                  if Payload'Length = 0 or else
                    Offset - Header_Byte_Count > Payload'Last - Payload'First
                  then Data := [others => 0]; return; end if;
                  Data (I) := Payload (Payload'First + (Offset - Header_Byte_Count));
               elsif Word (Offset) < Extent (Item) then
                  declare Source : constant Word := Word (Offset) - Header_Byte_Count - Word (Item.Payload_Bytes); begin
                     if Filter'Length = 0 or else Source > Word (Filter'Last - Filter'First)
                     then Data := [others => 0]; return; end if;
                     Data (I) := Filter (Filter'First + Natural (Source));
                  end;
               end if;
            end;
         end loop;
      end;
      if not Bytes_IO.ID_Text_Valid (Item.Context, Data) then Data := [others => 0]; return; end if;
      Written := Extent (Item); Accepted := True;
   end Encode_Envelope;
   function Read_Frame_Header (Data : Bytes_IO.Byte_Array; Limit : Node_Limit) return Header_Result
     with Post => (if Read_Frame_Header'Result.Accepted then
       Valid (Read_Frame_Header'Result.Value, Limit) and then
       Extent (Read_Frame_Header'Result.Value) <= Word (Data'Length));
   function Read_Frame_Header (Data : Bytes_IO.Byte_Array; Limit : Node_Limit) return Header_Result is
      Words_In : Header_Words := [others => 0];
   begin
      if Data'Length < Header_Byte_Count then return (Accepted => False); end if;
      for I in Words_In'Range loop
         declare R : constant Word_Result := Read_Word (Data, Word (I * Bytes_IO.Bytes_Per_Word)); begin
            if not R.Accepted then return (Accepted => False); end if;
            Words_In (I) := R.Value;
         end;
      end loop;
      declare R : constant Header_Result := Decode_Header (Words_In, Limit); begin
         if not R.Accepted or else Extent (R.Value) > Word (Data'Length) then return (Accepted => False); end if;
         return R;
      end;
   end Read_Frame_Header;
   function Read_Envelope (Data : Bytes_IO.Byte_Array; Limit : Node_Limit) return Header_Result is
      R : constant Header_Result := Read_Frame_Header (Data, Limit);
   begin
      if not R.Accepted then return R; end if;
      if not Bytes_IO.ID_Text_Valid (R.Value.Context, Data) then return (Accepted => False); end if;
         for I in Data'Range loop
            if Word (I - Data'First) >= Extent (R.Value) and then Data (I) /= 0 then
               return (Accepted => False);
            end if;
         end loop;
         return R;
   end Read_Envelope;
   function Node_At (Data : Bytes_IO.Byte_Array; Position : U32; Limit : Node_Limit)
     return Node_Result is
      H : constant Header_Result := Read_Frame_Header (Data, Limit);
   begin
      if not H.Accepted or else H.Value.Status /= OK
        or else H.Value.Context.Key.Kind not in Find_By_ID | Find_By_Address
        or else Position >= H.Value.Record_Count then return (Accepted => False); end if;
      declare R : constant Word_Result := Read_Word (Data, Header_Byte_Count + Word (Position) * Node_Byte_Count); begin
         if not R.Accepted or else R.Value = 0 or else R.Value > Word (Limit) then
            return (Accepted => False);
         end if;
         return (True, (H.Value.Context.Key.Instance, H.Value.Context.Key.Revision, Node_Index (R.Value)));
      end;
   end Node_At;
   function Resource_At (Data : Bytes_IO.Byte_Array; Position : U32; Limit : Node_Limit)
     return Resources_IO.Resource_Result is
      H : constant Header_Result := Read_Frame_Header (Data, Limit);
      Record_Data : Resources_IO.Record_Words := [others => 0];
   begin
      if not H.Accepted or else H.Value.Status /= OK or else H.Value.Context.Key.Kind /= Resources
        or else Position >= H.Value.Record_Count then return (Accepted => False); end if;
      for I in Record_Data'Range loop
         declare R : constant Word_Result := Read_Word
           (Data, Header_Byte_Count + Word (Position) * Resource_Byte_Count + Word (I * Bytes_IO.Bytes_Per_Word)); begin
            if not R.Accepted then return (Accepted => False); end if;
            Record_Data (I) := R.Value;
         end;
      end loop;
      declare R : constant Resources_IO.Resource_Result :=
        Resources_IO.Decode (Record_Data, Limit, H.Value.Payload_Bytes); begin
         if not R.Accepted then return R; end if;
         if R.Value.Controller.Kind = Resources_IO.Resolved_Node then
            if R.Value.Controller.Node.Instance /= H.Value.Context.Key.Instance
              or else R.Value.Controller.Node.Revision /= H.Value.Context.Key.Revision
            then return (Accepted => False); end if;
         elsif R.Value.Controller.Kind = Resources_IO.Unresolved_Path then
            if Word (R.Value.Controller.Path.Offset) < Word (H.Value.Record_Count) * Resource_Byte_Count
            then return (Accepted => False); end if;
         end if;
         return R;
      end;
   end Resource_At;
end ACPI_Passive_Query.Replies;
