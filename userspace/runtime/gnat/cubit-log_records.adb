pragma Ada_2022;
package body CuBit.Log_Records with SPARK_Mode is
   function Valid_Text (Text : String) return Boolean is
      Remaining : Natural range 0 .. 3 := 0;
      Low : Unsigned_8 := 16#80#;
      High : Unsigned_8 := 16#BF#;
      B : Unsigned_8;
   begin
      for C of Text loop
         B := Unsigned_8 (Character'Pos (C));
         if Remaining /= 0 then
            if B < Low or B > High then
               return False;
            end if;
            Remaining := Remaining - 1;
            Low := 16#80#;
            High := 16#BF#;
         elsif B in 16#20# .. 16#7E# or B = 9 then
            null;
         elsif B in 16#C2# .. 16#DF# then
            Remaining := 1;
         elsif B in 16#E0# .. 16#EF# then
            Remaining := 2;
            if B = 16#E0# then
               Low := 16#A0#;
            end if;
            if B = 16#ED# then
               High := 16#9F#;
            end if;
         elsif B in 16#F0# .. 16#F4# then
            Remaining := 3;
            if B = 16#F0# then
               Low := 16#90#;
            end if;
            if B = 16#F4# then
               High := 16#8F#;
            end if;
         else
            return False;
         end if;
      end loop;
      return Remaining = 0;
   end Valid_Text;

   function Make
     (Text : String; Level : Severity := Information;
      Reported_Time : Timestamp := (Clock => Unspecified)) return Decoded is
      Item : Log_Record;
   begin
      if Text'Length > Maximum_Text_Bytes then
         return (Success => False, Reason => Too_Long);
      elsif not Valid_Text (Text) then
         return (Success => False, Reason => Invalid_Text);
      end if;
      Item.Content (1 .. Text'Length) := Text;
      Item.Length := Text'Length;
      Item.Importance := Level;
      Item.Time := Reported_Time;
      return (Success => True, Value => Item);
   end Make;

   function Text (Item : Log_Record) return String is
     (Item.Content (1 .. Item.Length));
   function Level (Item : Log_Record) return Severity is (Item.Importance);
   function Reported_Time (Item : Log_Record) return Timestamp is (Item.Time);

   function Signed_Value (Item : Field) return Integer_64 is
     (if Item.Raw <= Unsigned_64 (Integer_64'Last)
      then Integer_64 (Item.Raw)
      else -Integer_64 (not Item.Raw) - 1);

   function Field_At (Item : Log_Record; Index : Field_Index) return Field is
     (Item.Fields (Index));

   function With_Field
     (Item : Log_Record; Name : String; Kind : Field_Kind;
      Value : Unsigned_64) return Decoded
   is
      Result : Log_Record := Item;
      Added : Field;
   begin
      if Item.Field_Used = Maximum_Fields then
         return (Success => False, Reason => Too_Many_Fields);
      elsif not Valid_Field_Name (Name) or else
        (Kind = Truth and then Value > 1)
      then
         return (Success => False, Reason => Invalid_Field);
      end if;
      for I in Name'Range loop
         Added.Label (I - Name'First + 1) := Name (I);
      end loop;
      Added.Label_Length := Name'Length;
      Added.Category := Kind;
      Added.Raw := Value;
      Result.Field_Used := Item.Field_Used + 1;
      Result.Fields (Result.Field_Used) := Added;
      return (Success => True, Value => Result);
   end With_Field;

   --  Wire layout (one-based byte offsets); see docs/typed-logging.md.
   --  ASCII "CLOG".
   subtype Magic_Range is Positive range 1 .. 4;
   function Has_Magic (Bytes : Wire_Buffer) return Boolean is
     (Bytes (Magic_Range) =
        [Character'Pos ('C'), Character'Pos ('L'), Character'Pos ('O'),
         Character'Pos ('G')]);
   Version_Byte : constant := 5;
   Severity_Byte : constant := 6;
   Clock_Byte : constant := 7;
   Flags_Byte : constant := 8;
   Length_Low_Byte : constant := 9;
   Length_High_Byte : constant := 10;
   Field_Count_Byte : constant := 11;
   First_Reserved_Byte : constant := 12;
   Last_Reserved_Byte : constant := 16;
   Time_Word : constant := 17;
   Domain_Word : constant := 25;
   Text_Only_Format : constant := 1;
   Field_Format : constant := 2;
   Byte_Radix : constant := 256;
   Word_Bytes : constant := 8;
   Bits_Per_Byte : constant := 8;
   Byte_Mask : constant Unsigned_64 := 16#FF#;
   --  Offsets within one field entry.
   Field_Kind_Offset : constant := 17;
   Field_Reserved_First : constant := 18;
   Field_Reserved_Last : constant := 24;
   Field_Value_Offset : constant := 25;

   subtype Word_Start is Positive range 1 .. Wire_Count'Last - Word_Bytes + 1;
   procedure Put_Word
     (Bytes : in out Wire_Buffer; First : Word_Start; Value : Unsigned_64);
   function Get_Word
     (Bytes : Wire_Buffer; First : Word_Start) return Unsigned_64;
   procedure Put_Word
     (Bytes : in out Wire_Buffer; First : Word_Start; Value : Unsigned_64) is
   begin
      for I in 0 .. Word_Bytes - 1 loop
         Bytes (First + I) :=
           Unsigned_8 (Shift_Right (Value, I * Bits_Per_Byte) and Byte_Mask);
      end loop;
   end Put_Word;
   function Get_Word
     (Bytes : Wire_Buffer; First : Word_Start) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in 0 .. Word_Bytes - 1 loop
         Value := Value or
           Shift_Left (Unsigned_64 (Bytes (First + I)), I * Bits_Per_Byte);
      end loop;
      return Value;
   end Get_Word;

   function Field_Base (Index : Field_Index) return Natural is
     (Header_Bytes + (Index - 1) * Field_Bytes);

   procedure Encode
     (Item : Log_Record; Bytes : out Wire_Buffer; Used : out Wire_Count) is
      Text_Start : constant Natural :=
        Header_Bytes + Item.Field_Used * Field_Bytes;
   begin
      Bytes := [others => 0];
      Bytes (Magic_Range) :=
        [Character'Pos ('C'), Character'Pos ('L'), Character'Pos ('O'),
         Character'Pos ('G')];
      Bytes (Version_Byte) :=
        (if Item.Field_Used = 0 then Text_Only_Format else Field_Format);
      Bytes (Severity_Byte) := Severity'Pos (Item.Importance);
      Bytes (Clock_Byte) := Timebase'Pos (Item.Time.Clock);
      Bytes (Length_Low_Byte) := Unsigned_8 (Item.Length mod Byte_Radix);
      Bytes (Length_High_Byte) := Unsigned_8 (Item.Length / Byte_Radix);
      Bytes (Field_Count_Byte) := Unsigned_8 (Item.Field_Used);
      case Item.Time.Clock is
         when Unspecified => null;
         when Monotonic_Milliseconds =>
            Put_Word (Bytes, Time_Word, Item.Time.Ticks);
            Put_Word (Bytes, Domain_Word, Item.Time.Domain);
         when Unix_Milliseconds =>
            Put_Word (Bytes, Time_Word, Item.Time.Since_Epoch);
      end case;
      for F in 1 .. Item.Field_Used loop
         declare
            Base : constant Natural := Field_Base (F);
            Entry_Field : constant Field := Item.Fields (F);
         begin
            for I in 1 .. Entry_Field.Label_Length loop
               Bytes (Base + I) := Character'Pos (Entry_Field.Label (I));
            end loop;
            Bytes (Base + Field_Kind_Offset) :=
              Field_Kind'Enum_Rep (Entry_Field.Category);
            Put_Word (Bytes, Base + Field_Value_Offset, Entry_Field.Raw);
         end;
      end loop;
      for I in 1 .. Item.Length loop
         Bytes (Text_Start + I) := Character'Pos (Item.Content (I));
      end loop;
      Used := Text_Start + Item.Length;
   end Encode;

   --  Decodes field Index from Bytes into Item; False when malformed.
   procedure Decode_Field
     (Bytes : Wire_Buffer; Index : Field_Index; Item : out Field;
      Valid : out Boolean);
   procedure Decode_Field
     (Bytes : Wire_Buffer; Index : Field_Index; Item : out Field;
      Valid : out Boolean)
   is
      Base : constant Natural := Field_Base (Index);
      Code : constant Unsigned_8 := Bytes (Base + Field_Kind_Offset);
      Found : Boolean := False;
   begin
      Item := (others => <>);
      Valid := False;
      for I in Field_Name_Index loop
         exit when Bytes (Base + I) = 0;
         Item.Label_Length := I;
      end loop;
      for I in Field_Name_Index loop
         if I <= Item.Label_Length then
            Item.Label (I) := Character'Val (Bytes (Base + I));
         elsif Bytes (Base + I) /= 0 then
            return;
         end if;
      end loop;
      if not Valid_Field_Name (Name (Item)) then
         return;
      end if;
      for K in Field_Kind loop
         if Unsigned_64 (Code) = Field_Kind'Enum_Rep (K) then
            Item.Category := K;
            Found := True;
         end if;
      end loop;
      if not Found or else
        (for some I in Field_Reserved_First .. Field_Reserved_Last =>
           Bytes (Base + I) /= 0)
      then
         return;
      end if;
      Item.Raw := Get_Word (Bytes, Base + Field_Value_Offset);
      Valid := Item.Category /= Truth or else Item.Raw <= 1;
   end Decode_Field;

   function Decode (Bytes : Wire_Buffer; Used : Wire_Count) return Decoded is
      Length : Natural;
      Count : Natural;
      Text_Start : Natural;
      Item : Log_Record;
      Ticks, Domain : Unsigned_64;
      Valid : Boolean;
   begin
      if Used < Header_Bytes then
         return (Success => False, Reason => Truncated);
      elsif not Has_Magic (Bytes) or else
        Bytes (Version_Byte) not in Text_Only_Format | Field_Format or else
        Bytes (Severity_Byte) > Severity'Pos (Severity'Last) or else
        Bytes (Clock_Byte) > Timebase'Pos (Timebase'Last) or else
        Bytes (Flags_Byte) /= 0 or else
        (for some I in First_Reserved_Byte .. Last_Reserved_Byte =>
           Bytes (I) /= 0)
      then
         return (Success => False, Reason => Invalid_Header);
      end if;
      Count := Natural (Bytes (Field_Count_Byte));
      --  Canonical: version 1 has no fields, version 2 at least one.
      if (Bytes (Version_Byte) = Text_Only_Format and then Count /= 0) or else
        (Bytes (Version_Byte) = Field_Format and then Count = 0)
      then
         return (Success => False, Reason => Invalid_Header);
      elsif Count > Maximum_Fields then
         return (Success => False, Reason => Too_Many_Fields);
      end if;
      Text_Start := Header_Bytes + Count * Field_Bytes;
      Length := Natural (Bytes (Length_Low_Byte)) +
        Natural (Bytes (Length_High_Byte)) * Byte_Radix;
      if Length > Maximum_Text_Bytes or else Used /= Text_Start + Length then
         return (Success => False, Reason => Length_Mismatch);
      end if;
      Ticks := Get_Word (Bytes, Time_Word);
      Domain := Get_Word (Bytes, Domain_Word);
      case Timebase'Val (Bytes (Clock_Byte)) is
         when Unspecified =>
            if Ticks /= 0 or Domain /= 0 then
               return (Success => False, Reason => Invalid_Timestamp);
            end if;
         when Monotonic_Milliseconds =>
            if Domain = 0 then
               return (Success => False, Reason => Invalid_Timestamp);
            end if;
            Item.Time := (Monotonic_Milliseconds, Domain, Ticks);
         when Unix_Milliseconds =>
            if Domain /= 0 then
               return (Success => False, Reason => Invalid_Timestamp);
            end if;
            Item.Time := (Unix_Milliseconds, Ticks);
      end case;
      for F in 1 .. Count loop
         Decode_Field (Bytes, F, Item.Fields (F), Valid);
         if not Valid then
            return (Success => False, Reason => Invalid_Field);
         end if;
      end loop;
      Item.Field_Used := Count;
      Item.Length := Length;
      Item.Importance := Severity'Val (Bytes (Severity_Byte));
      for I in 1 .. Item.Length loop
         Item.Content (I) := Character'Val (Bytes (Text_Start + I));
      end loop;
      if not Valid_Text (Text (Item)) then
         return (Success => False, Reason => Invalid_Text);
      end if;
      return (Success => True, Value => Item);
   end Decode;
end CuBit.Log_Records;
