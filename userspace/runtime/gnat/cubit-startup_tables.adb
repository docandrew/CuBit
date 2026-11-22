pragma Ada_2022;
package body CuBit.Startup_Tables with SPARK_Mode is

   function To_Name (Text : String) return Name is
      Result : Name;
   begin
      Result.Length := Text'Length;
      for I in 1 .. Text'Length loop
         Result.Text (I) := Text (Text'First + (I - 1));
         pragma Loop_Invariant
           (for all K in 1 .. I => Result.Text (K) = Text (Text'First + (K - 1)));
         pragma Loop_Invariant
           (for all K in Text'Length + 1 .. Maximum_Name_Bytes => Result.Text (K) = Pad);
      end loop;
      return Result;
   end To_Name;

   function Find (Item : Table; Label : Name) return Entry_Count is
   begin
      for I in 1 .. Item.Count loop
         if Item.Entries (I).Label = Label then
            return I;
         end if;
         pragma Loop_Invariant
           (for all K in 1 .. I => Item.Entries (K).Label /= Label);
      end loop;
      return 0;
   end Find;

   ---------------------------------------------------------------------------
   --  Encode
   ---------------------------------------------------------------------------
   procedure Encode (Item : Table; Output : out Bytes; Length : out Table_Length) is
      procedure Put_8 (Offset : Natural; Value : Unsigned_8)
      with Pre => Offset < Output'Length, Global => (In_Out => Output);
      procedure Put_8 (Offset : Natural; Value : Unsigned_8) is
      begin
         Output (Output'First + Offset) := Value;
      end Put_8;

      procedure Put_16 (Offset : Natural; Value : Unsigned_16)
      with Pre => Output'Length >= 2 and then Offset <= Output'Length - 2,
           Global => (In_Out => Output);
      procedure Put_16 (Offset : Natural; Value : Unsigned_16) is
      begin
         Put_8 (Offset, Unsigned_8 (Value and 16#FF#));
         Put_8 (Offset + 1, Unsigned_8 (Shift_Right (Value, 8)));
      end Put_16;

      procedure Put_32 (Offset : Natural; Value : Unsigned_32)
      with Pre => Output'Length >= 4 and then Offset <= Output'Length - 4,
           Global => (In_Out => Output);
      procedure Put_32 (Offset : Natural; Value : Unsigned_32) is
      begin
         for K in 0 .. 3 loop
            Put_8 (Offset + K, Unsigned_8 (Shift_Right (Value, 8 * K) and 16#FF#));
         end loop;
      end Put_32;

      procedure Put_64 (Offset : Natural; Value : Unsigned_64)
      with Pre => Output'Length >= 8 and then Offset <= Output'Length - 8,
           Global => (In_Out => Output);
      procedure Put_64 (Offset : Natural; Value : Unsigned_64) is
      begin
         for K in 0 .. 7 loop
            Put_8 (Offset + K, Unsigned_8 (Shift_Right (Value, 8 * K) and 16#FF#));
         end loop;
      end Put_64;

      Base : Natural;
   begin
      Output := [others => 0];
      Length := Encoded_Length (Item.Count);
      Put_32 (Magic_Offset, Magic);
      Put_16 (Version_Offset, Format_Version);
      Put_16 (Count_Offset, Unsigned_16 (Item.Count));
      Put_8 (Limit_Kind_Offset, Unsigned_8 (Limit_Kind'Enum_Rep (Item.Limit.Kind)));
      Put_32 (Limit_Count_Offset,
              (if Item.Limit.Kind = At_Most then Item.Limit.Count else 0));
      for I in 1 .. Item.Count loop
         pragma Loop_Invariant (Output'First = 1);
         Base := Header_Bytes + (I - 1) * Entry_Bytes;
         declare
            E : Startup_Entry renames Item.Entries (I);
         begin
            Put_8 (Base + Kind_Offset, Unsigned_8 (Capability_Kind'Enum_Rep (E.Kind)));
            Put_8 (Base + Need_Offset, Unsigned_8 (Need'Enum_Rep (E.Requirement)));
            Put_8 (Base + State_Offset, Unsigned_8 (Entry_State'Enum_Rep (E.State)));
            Put_8 (Base + Name_Length_Offset, Unsigned_8 (E.Label.Length));
            Put_64 (Base + Handle_Offset, E.Handle);
            for K in 1 .. E.Label.Length loop
               Put_8 (Base + Name_Offset + K - 1, Unsigned_8 (Character'Pos (E.Label.Text (K))));
            end loop;
         end;
      end loop;
   end Encode;

   ---------------------------------------------------------------------------
   --  Decode: every byte of Raw is read at most once, into locals; checks
   --  and the duplicate search run on the decoded copy.
   ---------------------------------------------------------------------------
   procedure Decode (Raw : Bytes; Result : out Table; Outcome : out Decode_Result) is

      function Get_8 (Offset : Natural) return Unsigned_8 is (Raw (Raw'First + Offset))
      with Pre => Offset < Raw'Length;

      function Get_16 (Offset : Natural) return Unsigned_16 is
        (Unsigned_16 (Get_8 (Offset)) or Shift_Left (Unsigned_16 (Get_8 (Offset + 1)), 8))
      with Pre => Raw'Length >= 2 and then Offset <= Raw'Length - 2;

      function Get_32 (Offset : Natural) return Unsigned_32 is
        (Unsigned_32 (Get_8 (Offset))
         or Shift_Left (Unsigned_32 (Get_8 (Offset + 1)), 8)
         or Shift_Left (Unsigned_32 (Get_8 (Offset + 2)), 16)
         or Shift_Left (Unsigned_32 (Get_8 (Offset + 3)), 24))
      with Pre => Raw'Length >= 4 and then Offset <= Raw'Length - 4;

      function Get_64 (Offset : Natural) return Unsigned_64 is
        (Unsigned_64 (Get_32 (Offset)) or Shift_Left (Unsigned_64 (Get_32 (Offset + 4)), 32))
      with Pre => Raw'Length >= 8 and then Offset <= Raw'Length - 8;

      procedure Refuse (Why : Decode_Result)
      with Pre  => Why /= Decoded,
           Post => Outcome = Why and then Result = Empty_Table,
           Global => (Output => (Outcome, Result));
      procedure Refuse (Why : Decode_Result) is
      begin
         Outcome := Why;
         Result := Empty_Table;
      end Refuse;

      Count : Natural;
      Kind_Code, Need_Code, State_Code, Length_Code : Unsigned_8;
      Limit_Code : Unsigned_8;
      Limit_Count : Unsigned_32;
      Base : Natural;
      Item : Startup_Entry;
      C : Character;
      Decoded_Table : Table := Empty_Table;
   begin
      Outcome := Decoded;

      --  Header.
      if Raw'Length < Header_Bytes or else Raw'Length > Maximum_Table_Bytes then
         Refuse (Wrong_Length);
         return;
      end if;
      if Get_32 (Magic_Offset) /= Magic then
         Refuse (Bad_Magic);
         return;
      end if;
      if Get_16 (Version_Offset) /= Format_Version then
         Refuse (Unknown_Version);
         return;
      end if;
      Count := Natural (Get_16 (Count_Offset));
      if Count > Maximum_Entries then
         Refuse (Too_Many_Entries);
         return;
      end if;
      if Raw'Length /= Encoded_Length (Count) then
         Refuse (Wrong_Length);
         return;
      end if;
      for K in 0 .. Header_Reserved_Bytes - 1 loop
         if Get_8 (Header_Reserved_First + K) /= 0 then
            Refuse (Bad_Reserved);
            return;
         end if;
      end loop;
      Limit_Code := Get_8 (Limit_Kind_Offset);
      Limit_Count := Get_32 (Limit_Count_Offset);
      if Limit_Code = Unsigned_8 (Limit_Kind'Enum_Rep (Standard_Limit)) and then Limit_Count = 0 then
         Decoded_Table.Limit := (Kind => Standard_Limit);
      elsif Limit_Code = Unsigned_8 (Limit_Kind'Enum_Rep (Kernel_Limit)) and then Limit_Count = 0 then
         Decoded_Table.Limit := (Kind => Kernel_Limit);
      elsif Limit_Code = Unsigned_8 (Limit_Kind'Enum_Rep (At_Most))
        and then Limit_Count in Handle_Count
      then
         Decoded_Table.Limit := (Kind => At_Most, Count => Limit_Count);
      else
         Refuse (Bad_Limit);
         return;
      end if;

      --  Entries.
      for I in 1 .. Count loop
         pragma Loop_Invariant (Decoded_Table.Count = I - 1);
         pragma Loop_Invariant (Raw'Length = Encoded_Length (Count));
         pragma Loop_Invariant
           (for all K in 1 .. I - 1 => Valid_Entry (Decoded_Table.Entries (K)));
         pragma Loop_Invariant (Distinct_Names (Decoded_Table));
         pragma Loop_Invariant (Outcome = Decoded);

         Base := Header_Bytes + (I - 1) * Entry_Bytes;
         Item := (others => <>);
         Kind_Code := Get_8 (Base + Kind_Offset);
         Need_Code := Get_8 (Base + Need_Offset);
         State_Code := Get_8 (Base + State_Offset);
         Length_Code := Get_8 (Base + Name_Length_Offset);

         if Kind_Code < Unsigned_8 (Capability_Kind'Enum_Rep (Capability_Kind'First))
           or else Kind_Code > Unsigned_8 (Capability_Kind'Enum_Rep (Capability_Kind'Last))
         then
            Refuse (Bad_Kind);
            return;
         end if;
         for K in Capability_Kind loop
            if Kind_Code = Unsigned_8 (Capability_Kind'Enum_Rep (K)) then
               Item.Kind := K;
            end if;
         end loop;

         if Need_Code = Unsigned_8 (Need'Enum_Rep (Required)) then
            Item.Requirement := Required;
         elsif Need_Code = Unsigned_8 (Need'Enum_Rep (Optional)) then
            Item.Requirement := Optional;
         else
            Refuse (Bad_Need);
            return;
         end if;

         if State_Code = Unsigned_8 (Entry_State'Enum_Rep (Unassigned)) then
            Item.State := Unassigned;
         elsif State_Code = Unsigned_8 (Entry_State'Enum_Rep (Granted)) then
            Item.State := Granted;
         elsif State_Code = Unsigned_8 (Entry_State'Enum_Rep (Absent)) then
            Item.State := Absent;
         else
            Refuse (Bad_State);
            return;
         end if;

         for K in 0 .. Entry_Reserved_Bytes - 1 loop
            if Get_8 (Base + Entry_Reserved_First + K) /= 0 then
               Refuse (Bad_Reserved);
               return;
            end if;
         end loop;

         Item.Handle := Get_64 (Base + Handle_Offset);
         if (case Item.State is
               when Unassigned => Item.Handle /= No_Handle,
               when Granted    => Item.Handle = No_Handle,
               when Absent     => Item.Handle /= No_Handle
                                  or else Item.Requirement /= Optional)
         then
            Refuse (Bad_Handle);
            return;
         end if;

         --  The name: Length_Code characters, then padding.
         if Length_Code < 1 or else Length_Code > Maximum_Name_Bytes then
            Refuse (Bad_Name);
            return;
         end if;
         Item.Label.Length := Natural (Length_Code);
         for K in 1 .. Maximum_Name_Bytes loop
            pragma Loop_Invariant (Item.Label.Length = Natural (Length_Code));
            pragma Loop_Invariant
              (for all P in 1 .. K - 1 =>
                 (if P <= Item.Label.Length then Name_Character (Item.Label.Text (P))
                  else Item.Label.Text (P) = Pad));
            pragma Loop_Invariant
              (for all P in 2 .. Natural'Min (K - 1, Item.Label.Length) =>
                 not (Item.Label.Text (P) = '-' and then Item.Label.Text (P - 1) = '-'));
            pragma Loop_Invariant (if K > 1 then Item.Label.Text (1) in 'a' .. 'z');
            pragma Loop_Invariant
              (if K > Item.Label.Length then Item.Label.Text (Item.Label.Length) /= '-');
            C := Character'Val (Get_8 (Base + Name_Offset + K - 1));
            if K <= Item.Label.Length then
               if not Name_Character (C)
                 or else (K = 1 and then C not in 'a' .. 'z')
                 or else (K = Item.Label.Length and then C = '-')
                 or else (K > 1 and then C = '-' and then Item.Label.Text (K - 1) = '-')
               then
                  Refuse (Bad_Name);
                  return;
               end if;
            elsif C /= Pad then
               Refuse (Bad_Name);
               return;
            end if;
            Item.Label.Text (K) := C;
         end loop;
         pragma Assert (Valid_Name (Item.Label));
         pragma Assert (Valid_Entry (Item));

         for K in 1 .. I - 1 loop
            pragma Loop_Invariant
              (for all P in 1 .. K - 1 => Decoded_Table.Entries (P).Label /= Item.Label);
            if Decoded_Table.Entries (K).Label = Item.Label then
               Refuse (Duplicate_Name);
               return;
            end if;
         end loop;

         Decoded_Table.Entries (I) := Item;
         Decoded_Table.Count := I;
      end loop;

      Result := Decoded_Table;
   end Decode;

end CuBit.Startup_Tables;
