------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Channel_Declarations with SPARK_Mode is

   procedure Put_16 (Output : in out Bytes; At_Byte : Positive; Value : Unsigned_16)
   with Pre => At_Byte in Output'Range and then At_Byte < Output'Last;
   procedure Put_16 (Output : in out Bytes; At_Byte : Positive; Value : Unsigned_16) is
   begin
      Output (At_Byte) := Unsigned_8 (Value and 16#FF#);
      Output (At_Byte + 1) := Unsigned_8 (Shift_Right (Value, 8));
   end Put_16;

   procedure Put_32 (Output : in out Bytes; At_Byte : Positive; Value : Unsigned_32)
   with Pre => At_Byte in Output'Range and then At_Byte <= Output'Last - 3;
   procedure Put_32 (Output : in out Bytes; At_Byte : Positive; Value : Unsigned_32) is
   begin
      for K in 0 .. 3 loop
         Output (At_Byte + K) := Unsigned_8 (Shift_Right (Value, 8 * K) and 16#FF#);
      end loop;
   end Put_32;

   function Count_Word (Limit : Channel_Limit) return Unsigned_32 is
     (case Limit.Kind is
         when At_Most      => Unsigned_32 (Limit.Count),
         when Server_Limit => 0);

   procedure Encode (Item : Table; Output : out Bytes; Length : out Table_Length) is
   begin
      Output := [others => 0];
      Length := Encoded_Length (Item.Count);
      Put_32 (Output, 1 + Magic_Offset, Magic);
      Put_16 (Output, 1 + Version_Offset, Format_Version);
      Put_16 (Output, 1 + Count_Offset, Unsigned_16 (Item.Count));
      for I in 1 .. Item.Count loop
         declare
            Base : constant Positive := 1 + Header_Bytes + (I - 1) * Offer_Bytes;
            O : Offer renames Item.Offers (I);
         begin
            Output (Base + Length_Offset) := Unsigned_8 (O.Contract.Length);
            Output (Base + Class_Offset) := Unsigned_8 (Channel_Class'Enum_Rep (O.Class));
            Output (Base + Per_Kind_Offset) := Unsigned_8 (Limit_Kind'Enum_Rep (O.Per_Client.Kind));
            Output (Base + Total_Kind_Offset) := Unsigned_8 (Limit_Kind'Enum_Rep (O.Total.Kind));
            Put_32 (Output, Base + Per_Count_Offset, Count_Word (O.Per_Client));
            Put_32 (Output, Base + Total_Count_Offset, Count_Word (O.Total));
            for K in 1 .. O.Contract.Length loop
               Output (Base + Contract_Offset + K - 1) := Character'Pos (O.Contract.Text (K));
            end loop;
         end;
      end loop;
   end Encode;

   function Get_16 (Raw : Bytes; At_Byte : Positive) return Unsigned_16 is
     (Unsigned_16 (Raw (At_Byte)) or Shift_Left (Unsigned_16 (Raw (At_Byte + 1)), 8))
   with Pre => At_Byte in Raw'Range and then At_Byte < Raw'Last;

   function Get_32 (Raw : Bytes; At_Byte : Positive) return Unsigned_32 is
     (Unsigned_32 (Raw (At_Byte))
      or Shift_Left (Unsigned_32 (Raw (At_Byte + 1)), 8)
      or Shift_Left (Unsigned_32 (Raw (At_Byte + 2)), 16)
      or Shift_Left (Unsigned_32 (Raw (At_Byte + 3)), 24))
   with Pre => At_Byte in Raw'Range and then At_Byte <= Raw'Last - 3;

   --  Whether a kind byte and count word make a limit.
   function Limit_Check (Kind : Unsigned_8; Count : Unsigned_32) return Decode_Result is
     (if Kind = Limit_Kind'Enum_Rep (At_Most) then
        (if Count in 1 .. Maximum_Total then Decoded else Bad_Count_Value)
      elsif Kind = Limit_Kind'Enum_Rep (Server_Limit) then
        (if Count = 0 then Decoded else Bad_Count_Value)
      else Bad_Kind);

   function Make_Limit (Kind : Unsigned_8; Count : Unsigned_32) return Channel_Limit is
     (if Kind = Limit_Kind'Enum_Rep (At_Most)
      then (Kind => At_Most, Count => Limit_Count (Count))
      else (Kind => Server_Limit))
   with Pre => Limit_Check (Kind, Count) = Decoded;

   procedure Decode (Raw : Bytes; Result : out Table; Outcome : out Decode_Result) is
      Count : Natural;
      Item : Offer;
      Class_Byte : Unsigned_8;
   begin
      Result := (Count => 0, Offers => [others => (others => <>)]);
      if Raw'Length < Header_Bytes then
         Outcome := Too_Short;
         return;
      end if;
      if Get_32 (Raw, 1 + Magic_Offset) /= Magic then
         Outcome := Bad_Magic;
         return;
      end if;
      if Get_16 (Raw, 1 + Version_Offset) /= Format_Version then
         Outcome := Bad_Version;
         return;
      end if;
      Count := Natural (Get_16 (Raw, 1 + Count_Offset));
      if Count > Maximum_Offers then
         Outcome := Bad_Count;
         return;
      end if;
      if Raw'Length /= Encoded_Length (Count) then
         Outcome := Bad_Length;
         return;
      end if;
      for I in 1 .. Count loop
         pragma Loop_Invariant (Result.Count = I - 1);
         pragma Loop_Invariant (Valid_Table (Result));
         declare
            Base : constant Positive := 1 + Header_Bytes + (I - 1) * Offer_Bytes;
            Length : constant Natural := Natural (Raw (Base + Length_Offset));
         begin
            Item := (others => <>);
            if Length not in 1 .. Maximum_Contract_Bytes then
               Outcome := Bad_Contract;
               return;
            end if;
            for K in 1 .. Maximum_Contract_Bytes loop
               pragma Loop_Invariant
                 (for all J in 1 .. K - 1 =>
                    (if J <= Length then Contract_Character (Item.Contract.Text (J))
                     else Item.Contract.Text (J) = Character'Val (0)));
               declare
                  B : constant Unsigned_8 := Raw (Base + Contract_Offset + K - 1);
               begin
                  if K <= Length then
                     if not Contract_Character (Character'Val (B)) then
                        Outcome := Bad_Contract;
                        return;
                     end if;
                  elsif B /= 0 then
                     Outcome := Bad_Contract;
                     return;
                  end if;
                  Item.Contract.Text (K) := Character'Val (B);
               end;
            end loop;
            Item.Contract.Length := Length;
            for K in 0 .. Reserved_Bytes - 1 loop
               if Raw (Base + Reserved_First + K) /= 0 then
                  Outcome := Reserved_Not_Zero;
                  return;
               end if;
            end loop;
            Class_Byte := Raw (Base + Class_Offset);
            if Class_Byte = Channel_Class'Enum_Rep (Urgent) then
               Item.Class := Urgent;
            elsif Class_Byte = Channel_Class'Enum_Rep (Interactive) then
               Item.Class := Interactive;
            elsif Class_Byte = Channel_Class'Enum_Rep (Normal) then
               Item.Class := Normal;
            else
               Outcome := Bad_Class;
               return;
            end if;
            declare
               Per_Kind : constant Unsigned_8 := Raw (Base + Per_Kind_Offset);
               Per_Count : constant Unsigned_32 := Get_32 (Raw, Base + Per_Count_Offset);
               Total_Kind : constant Unsigned_8 := Raw (Base + Total_Kind_Offset);
               Total_Count : constant Unsigned_32 := Get_32 (Raw, Base + Total_Count_Offset);
            begin
               Outcome := Limit_Check (Per_Kind, Per_Count);
               if Outcome /= Decoded then
                  return;
               end if;
               Outcome := Limit_Check (Total_Kind, Total_Count);
               if Outcome /= Decoded then
                  return;
               end if;
               Item.Per_Client := Make_Limit (Per_Kind, Per_Count);
               Item.Total := Make_Limit (Total_Kind, Total_Count);
            end;
            if not Consistent (Item) then
               Outcome := Inconsistent;
               return;
            end if;
            for J in 1 .. I - 1 loop
               pragma Loop_Invariant
                 (for all P in 1 .. J - 1 => Result.Offers (P).Contract /= Item.Contract);
               if Result.Offers (J).Contract = Item.Contract then
                  Outcome := Duplicate_Contract;
                  return;
               end if;
            end loop;
            Result.Offers (I) := Item;
            Result.Count := I;
         end;
      end loop;
      Outcome := Decoded;
   end Decode;

end CuBit.Channel_Declarations;
