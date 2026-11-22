pragma Ada_2022;
package body ACPI_Passive_Query with SPARK_Mode is
   use Interfaces;
   Byte_Bits : constant := 8;
   Byte_Mask : constant Word := 16#FF#;
   Key_Word_Count : constant := 7;
   subtype Key_Words is Word_Array (0 .. Key_Word_Count - 1);
   External_ID_Flag : constant Word := 2 ** 63;
   ID_Length_Mask : constant Word := Word (Unsigned_32'Last);

   function Upper (C : Character) return Character is
     (if C in 'a' .. 'z' then Character'Val (Character'Pos (C) -
        (Character'Pos ('a') - Character'Pos ('A'))) else C);
   function ID_Character (Item : Device_ID; Position : Positive) return Character
   is (Character'Val (Shift_Right (Item.Data, Byte_Bits * (Position - 1)) and Byte_Mask))
     with Pre => Position <= Maximum_ID_Length;
   function Valid_ID (Item : Device_ID) return Boolean is
   begin
      if Item.Storage /= Inline_ID or else Item.Length not in 7 | 8 then return False; end if;
      if Item.Length = 7 and then Shift_Right (Item.Data, 7 * Byte_Bits) /= 0 then
         return False;
      end if;
      for I in 1 .. Natural (Item.Length) loop
         declare C : constant Character := ID_Character (Item, I); begin
            if I <= Natural (Item.Length) - 4 then
               if C not in 'A' .. 'Z' and then
                 not (Item.Length = 8 and then C in '0' .. '9')
               then return False; end if;
            elsif C not in '0' .. '9' | 'A' .. 'F' then return False;
            end if;
         end;
      end loop;
      return True;
   end Valid_ID;
   function From_Text (Text : String) return ID_Result is
      Item : Device_ID;
   begin
      if Text'Length not in 7 | 8 then return (Accepted => False); end if;
      Item.Length := ID_Length (Text'Length);
      for I in 0 .. Text'Length - 1 loop
         Item.Data := Item.Data or Shift_Left
           (Word (Character'Pos (Upper (Text (Text'First + I)))), Byte_Bits * I);
      end loop;
      if Valid_ID (Item) then return (True, Item); end if;
      return (Accepted => False);
   end From_Text;
   function Image (Item : Device_ID) return String is
      Text : String (1 .. Natural (Item.Length));
   begin
      for I in Text'Range loop Text (I) := ID_Character (Item, I); end loop;
      return Text;
   end Image;
   function External_ID (Offset, Length : Unsigned_32) return Device_ID is
     ((ID_Length (Length), Word (Offset), External_Text_ID));
   function Is_External (Item : Device_ID) return Boolean is
     (Item.Storage = External_Text_ID and then Item.Data <= ID_Length_Mask);
   function Text_Offset (Item : Device_ID) return Unsigned_32 is (Unsigned_32 (Item.Data));
   function Text_Length (Item : Device_ID) return Unsigned_32 is (Unsigned_32 (Item.Length));
   function Valid_Query_ID (Item : Device_ID) return Boolean is
     (if Is_External (Item) then Item.Length > 0 and then Item.Data <= ID_Length_Mask
      else Valid_ID (Item));
   function Same_Filter_Metadata (Left, Right : Query_Key) return Boolean is
     (Left.Kind = Right.Kind and then Left.Instance = Right.Instance
      and then Left.Revision = Right.Revision and then Left.Node = Right.Node
      and then Left.Address = Right.Address
      and then Left.ID.Storage = Right.ID.Storage and then Left.ID.Length = Right.ID.Length
      and then (Is_External (Left.ID) or else Left.ID.Data = Right.ID.Data));
   function Valid_Key (Item : Query_Key; Limit : Node_Limit) return Boolean is
   begin
      case Item.Kind is
         when Find_By_ID =>
            return Valid_Query_ID (Item.ID) and then Item.Node = 0 and then Item.Address = 0
              and then ((Item.Instance = 0 and then Item.Revision = 0) or else
                        (Item.Instance /= 0 and then Item.Revision /= 0));
         when Find_By_Address | Describe | Resources =>
            return Item.Instance /= 0 and then Item.Revision /= 0
              and then Item.Node in 1 .. Limit and then Item.ID = No_ID
              and then (Item.Kind = Find_By_Address or else Item.Address = 0);
      end case;
   end Valid_Key;
   function Valid_Cursor (Item : Cursor; Limit : Node_Limit) return Boolean is
   begin
      if not Valid_Key (Item.Filter, Limit) or else Item.Filter.Instance = 0
        or else Item.Filter.Revision = 0 then return False; end if;
      case Item.Filter.Kind is
         when Find_By_ID | Find_By_Address =>
            return Item.Part = Node_Scan and then Item.Next_Node in 1 .. Limit
              and then Item.Offset = 0 and then Item.Subentry = 0;
         when Describe =>
            return Item.Part in Description_Path | Description_CID | Description_UID
              and then Item.Next_Node = 0 and then Item.Subentry = 0
              and then (Item.Part /= Description_Path or else
                        Item.Offset <= Unsigned_32 (Limit));
         when Resources =>
            return Item.Part = Resource_Subentry and then Item.Next_Node = 0;
      end case;
   end Valid_Cursor;
   function Valid (Item : Request; Limit : Node_Limit) return Boolean is
     (Valid_Key (Item.Key, Limit) and then
       (not Item.Continuation.Present or else
          (Same_Filter_Metadata (Item.Continuation.Value.Filter, Item.Key) and then
           Valid_Cursor (Item.Continuation.Value, Limit))));
   function Operation_Code (Item : Operation) return Word is
     (case Item is when Find_By_ID => 1, when Find_By_Address => 2,
       when Describe => 3, when Resources => 4);
   function Position_Code (Item : Position_Kind) return Word is
     (case Item is when Node_Scan => 1, when Description_Path => 2,
       when Description_CID => 3, when Description_UID => 4,
       when Resource_Subentry => 5);
   function Encode_Key (Item : Query_Key) return Key_Words is
     ([Operation_Code (Item.Kind), Word (Item.Instance), Word (Item.Revision),
       Word (Item.Node), Item.Address,
       (if Is_External (Item.ID) then External_ID_Flag or Word (Item.ID.Length) else Word (Item.ID.Length)),
       Item.ID.Data]);
   function Encode (Item : Request; Limit : Node_Limit) return Words is
      pragma Unreferenced (Limit);
      Data : Words := [others => 0];
   begin
      Data (0) := Frame_Magic; Data (1) := Frame_Version;
      declare Key : constant Key_Words := Encode_Key (Item.Key); begin
         for I in Key'Range loop Data (2 + I) := Key (I); end loop;
      end;
      if Item.Continuation.Present then
         Data (9) := 1;
         declare Key : constant Key_Words := Encode_Key (Item.Continuation.Value.Filter); begin
            for I in Key'Range loop Data (10 + I) := Key (I); end loop;
         end;
         Data (17) := Position_Code (Item.Continuation.Value.Part);
         Data (18) := Word (Item.Continuation.Value.Next_Node);
         Data (19) := Word (Item.Continuation.Value.Offset);
         Data (20) := Word (Item.Continuation.Value.Subentry);
      end if;
      return Data;
   end Encode;
   type Key_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Query_Key;
         when False => null;
      end case;
   end record;
   function Decode_Key (Data : Key_Words; Limit : Node_Limit) return Key_Result is
      Item : Query_Key;
   begin
      if Data (0) not in 1 .. 4 or else Data (2) > Word (Generation'Last)
        or else Data (3) > Word (Node_Index'Last)
        or else (Data (5) and not (External_ID_Flag or ID_Length_Mask)) /= 0
        or else ((Data (5) and External_ID_Flag) = 0 and then Data (5) > Maximum_ID_Length)
        or else ((Data (5) and External_ID_Flag) /= 0 and then Data (6) > ID_Length_Mask)
      then return (Accepted => False); end if;
      Item :=
        (Kind => (if Data (0) = 1 then Find_By_ID elsif Data (0) = 2 then Find_By_Address
                  elsif Data (0) = 3 then Describe else Resources),
         Instance => Instance_ID (Data (1)), Revision => Generation (Data (2)),
         Node => Node_Index (Data (3)), Address => Data (4),
         ID => (ID_Length (Data (5) and ID_Length_Mask), Data (6),
                (if (Data (5) and External_ID_Flag) = 0 then Inline_ID else External_Text_ID)));
      if Valid_Key (Item, Limit) then return (True, Item); end if;
      return (Accepted => False);
   end Decode_Key;
   function Decode (Data : Words; Limit : Node_Limit) return Decode_Result is
      Key : constant Key_Result := Decode_Key (Key_Words (Data (2 .. 8)), Limit);
      Other : constant Key_Result := Decode_Key (Key_Words (Data (10 .. 16)), Limit);
      Item : Request;
   begin
      if Data (0) /= Frame_Magic then return (Status => Malformed); end if;
      if Data (1) /= Frame_Version then return (Status => Unsupported_Version); end if;
      if not Key.Accepted or else Data (9) > 1 or else
        Data (21 .. 23) /= [0, 0, 0] then return (Status => Malformed); end if;
      Item.Key := Key.Value;
      if Data (9) = 0 then
         if Data (10 .. 20) /= [10 .. 20 => 0] then return (Status => Malformed); end if;
      else
         if not Other.Accepted or else Data (17) not in 1 .. 5
           or else Data (18) > Word (Node_Index'Last)
           or else Data (19) > Word (Unsigned_32'Last)
           or else Data (20) > Word (Unsigned_32'Last)
         then return (Status => Malformed); end if;
         Item.Continuation := (True,
           (Filter => Other.Value,
            Part => (if Data (17) = 1 then Node_Scan elsif Data (17) = 2 then Description_Path
                     elsif Data (17) = 3 then Description_CID elsif Data (17) = 4 then Description_UID
                     else Resource_Subentry),
            Next_Node => Node_Index (Data (18)), Offset => Unsigned_32 (Data (19)),
            Subentry => Unsigned_32 (Data (20))));
      end if;
      if Valid (Item, Limit) and then Encode (Item, Limit) = Data then
         return (Decoded, Item);
      end if;
      return (Status => Malformed);
   end Decode;
end ACPI_Passive_Query;
