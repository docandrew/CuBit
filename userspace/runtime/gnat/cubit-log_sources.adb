------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
package body CuBit.Log_Sources with SPARK_Mode is

   WORD_BYTES : constant := 8;
   BYTE_BITS  : constant := 8;
   KIND_AT            : constant := 0;
   IDENTITY_LENGTH_AT : constant := 1;
   NAME_LENGTH_AT     : constant := 2;
   SOURCE_AT          : constant := 8;
   SEEN_AT            : constant := 16;
   IDENTITY_AT        : constant := 24;
   NAME_AT            : constant := IDENTITY_AT + Identity_Bytes;
   --  Zero: the header's reserved bytes, and the tail after the name.
   HEADER_RESERVED_FIRST : constant := NAME_LENGTH_AT + 1;
   HEADER_RESERVED_LAST  : constant := SOURCE_AT - 1;
   TAIL_FIRST            : constant := NAME_AT + Name_Bytes;

   function Make (Source : Unsigned_64; Identity, Name : String) return Source_Identity is
      Result : Source_Identity := (Source => Source, others => <>);
      --  The file name: after the last '/'.
      Base : Integer := Name'First;
   begin
      for I in Name'Range loop
         if Name (I) = '/' then
            Base := I + 1;
         end if;
         pragma Loop_Invariant (Base >= Name'First);
      end loop;
      for I in Identity'Range loop
         if Text_Character (Identity (I)) and then Result.Identity_Used < Identity_Bytes then
            Result.Identity_Used := Result.Identity_Used + 1;
            Result.Identity (Result.Identity_Used) := Identity (I);
         end if;
         pragma Loop_Invariant (Result.Source = Source and then Result.Name_Used = 0);
         pragma Loop_Invariant
           (for all J in 1 .. Result.Identity_Used => Text_Character (Result.Identity (J)));
      end loop;
      for I in Base .. Name'Last loop
         if Text_Character (Name (I)) and then Result.Name_Used < Name_Bytes then
            Result.Name_Used := Result.Name_Used + 1;
            Result.Name (Result.Name_Used) := Name (I);
         end if;
         pragma Loop_Invariant (Result.Source = Source);
         pragma Loop_Invariant
           (for all J in 1 .. Result.Identity_Used => Text_Character (Result.Identity (J)));
         pragma Loop_Invariant
           (for all J in 1 .. Result.Name_Used => Text_Character (Result.Name (J)));
      end loop;
      return Result;
   end Make;

   function Label (Item : Source_Identity) return String is
   begin
      if Item.Name_Used > 0 then
         for I in 2 .. Item.Name_Used loop
            if Item.Name (I) = '.' then
               return Item.Name (1 .. I - 1);
            end if;
         end loop;
         return Item.Name (1 .. Item.Name_Used);
      end if;
      for I in reverse 1 .. Item.Identity_Used - 1 loop
         if Item.Identity (I) = '.' then
            return Item.Identity (I + 1 .. Item.Identity_Used);
         end if;
      end loop;
      return Item.Identity (1 .. Item.Identity_Used);
   end Label;

   --  Text ends with '.' and then Part.
   function Ends_With_Part (Text, Part : String) return Boolean is
     (Part'Length > 0 and then Text'Length > Part'Length
      and then Text (Text'Last - Part'Length + 1 .. Text'Last) = Part
      and then Text (Text'Last - Part'Length) = '.')
   with Pre => Text'First = 1 and then Text'Last <= Identity_Bytes;

   function Answers_To (Item : Source_Identity; Wanted : String) return Boolean is
     (Wanted'Length > 0
      and then (Name_Of (Item) = Wanted or else Label (Item) = Wanted
                or else Identity_Of (Item) = Wanted
                or else Ends_With_Part (Identity_Of (Item), Wanted)));

   procedure Put_Word (Into : in out Entry_Buffer; At_Byte : Entry_Position; Value : Unsigned_64)
   with Pre => At_Byte <= Entry_Bytes - WORD_BYTES;
   procedure Put_Word (Into : in out Entry_Buffer; At_Byte : Entry_Position; Value : Unsigned_64) is
   begin
      for I in 0 .. WORD_BYTES - 1 loop
         Into (At_Byte + I) := Unsigned_8 (Shift_Right (Value, I * BYTE_BITS) and 16#FF#);
      end loop;
   end Put_Word;

   function Word (From : Entry_Buffer; At_Byte : Entry_Position) return Unsigned_64
   with Pre => At_Byte <= Entry_Bytes - WORD_BYTES;
   function Word (From : Entry_Buffer; At_Byte : Entry_Position) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. WORD_BYTES - 1 loop
         Value := Shift_Left (Value, BYTE_BITS) or Unsigned_64 (From (At_Byte + I));
      end loop;
      return Value;
   end Word;

   procedure Encode (Item : Source_Entry; Into : out Entry_Buffer) is
   begin
      Into := [others => 0];
      Into (KIND_AT) := Unsigned_8 (Entry_Kind'Enum_Rep (Item.Kind));
      Into (IDENTITY_LENGTH_AT) := Unsigned_8 (Item.Who.Identity_Used);
      Into (NAME_LENGTH_AT) := Unsigned_8 (Item.Who.Name_Used);
      Put_Word (Into, SOURCE_AT, Item.Who.Source);
      Put_Word (Into, SEEN_AT, Item.Seen_Ms);
      for I in 1 .. Item.Who.Identity_Used loop
         Into (IDENTITY_AT + I - 1) := Character'Pos (Item.Who.Identity (I));
      end loop;
      for I in 1 .. Item.Who.Name_Used loop
         Into (NAME_AT + I - 1) := Character'Pos (Item.Who.Name (I));
      end loop;
   end Encode;

   procedure Decode (From : Entry_Buffer; Item : out Source_Entry; Accepted : out Boolean) is
      Kind_Byte     : constant Unsigned_8 := From (KIND_AT);
      Identity_Byte : constant Unsigned_8 := From (IDENTITY_LENGTH_AT);
      Name_Byte     : constant Unsigned_8 := From (NAME_LENGTH_AT);
      Who : Source_Identity;
      C : Character;
   begin
      Item := (others => <>);
      Accepted := False;
      if Kind_Byte not in Entry_Kind'Enum_Rep (Started) | Entry_Kind'Enum_Rep (Exited)
        or else Identity_Byte > Identity_Bytes or else Name_Byte > Name_Bytes
        or else (for some I in HEADER_RESERVED_FIRST .. HEADER_RESERVED_LAST => From (I) /= 0)
        or else (for some I in TAIL_FIRST .. Entry_Position'Last => From (I) /= 0)
      then
         return;
      end if;
      Who.Source := Word (From, SOURCE_AT);
      Who.Identity_Used := Natural (Identity_Byte);
      Who.Name_Used := Natural (Name_Byte);
      --  The text, and nothing but zeroes after it.
      for I in 1 .. Identity_Bytes loop
         if I <= Who.Identity_Used then
            C := Character'Val (From (IDENTITY_AT + I - 1));
            if not Text_Character (C) then
               return;
            end if;
            Who.Identity (I) := C;
         elsif From (IDENTITY_AT + I - 1) /= 0 then
            return;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. Natural'Min (I, Who.Identity_Used) =>
              Text_Character (Who.Identity (J)));
      end loop;
      for I in 1 .. Name_Bytes loop
         if I <= Who.Name_Used then
            C := Character'Val (From (NAME_AT + I - 1));
            if not Text_Character (C) then
               return;
            end if;
            Who.Name (I) := C;
         elsif From (NAME_AT + I - 1) /= 0 then
            return;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. Who.Identity_Used => Text_Character (Who.Identity (J)));
         pragma Loop_Invariant
           (for all J in 1 .. Natural'Min (I, Who.Name_Used) => Text_Character (Who.Name (J)));
      end loop;
      if Who.Source = No_Source
        or else (Kind_Byte = Entry_Kind'Enum_Rep (Started) and then not Named (Who))
      then
         return;
      end if;
      Item := (Kind => (if Kind_Byte = Entry_Kind'Enum_Rep (Started) then Started else Exited),
               Who => Who, Seen_Ms => Word (From, SEEN_AT));
      Accepted := True;
   end Decode;

end CuBit.Log_Sources;
