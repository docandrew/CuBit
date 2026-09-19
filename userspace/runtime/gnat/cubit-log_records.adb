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

   subtype Word_Start is Positive range 17 .. 25;
   procedure Put_Word
     (Bytes : in out Wire_Buffer; First : Word_Start; Value : Unsigned_64);
   function Get_Word
     (Bytes : Wire_Buffer; First : Word_Start) return Unsigned_64;
   procedure Put_Word
     (Bytes : in out Wire_Buffer; First : Word_Start; Value : Unsigned_64) is
   begin
      for I in 0 .. 7 loop
         Bytes (First + I) :=
           Unsigned_8 (Shift_Right (Value, I * 8) and 16#FF#);
      end loop;
   end Put_Word;
   function Get_Word
     (Bytes : Wire_Buffer; First : Word_Start) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         Value := Value or Shift_Left (Unsigned_64 (Bytes (First + I)), I * 8);
      end loop;
      return Value;
   end Get_Word;

   procedure Encode
     (Item : Log_Record; Bytes : out Wire_Buffer; Used : out Wire_Count) is
   begin
      Bytes := [others => 0];
      Bytes (1 .. 4) := [16#43#, 16#4C#, 16#4F#, 16#47#]; -- CLOG
      Bytes (5) := 1;
      Bytes (6) := Severity'Pos (Item.Importance);
      Bytes (7) := Timebase'Pos (Item.Time.Clock);
      Bytes (9) := Unsigned_8 (Item.Length mod 256);
      Bytes (10) := Unsigned_8 (Item.Length / 256);
      case Item.Time.Clock is
         when Unspecified => null;
         when Monotonic_Milliseconds =>
            Put_Word (Bytes, 17, Item.Time.Ticks);
            Put_Word (Bytes, 25, Item.Time.Domain);
         when Unix_Milliseconds =>
            Put_Word (Bytes, 17, Item.Time.Since_Epoch);
      end case;
      for I in 1 .. Item.Length loop
         Bytes (Header_Bytes + I) := Character'Pos (Item.Content (I));
      end loop;
      Used := Header_Bytes + Item.Length;
   end Encode;

   function Decode (Bytes : Wire_Buffer; Used : Wire_Count) return Decoded is
      Length : Natural;
      Item : Log_Record;
      Ticks, Domain : Unsigned_64;
   begin
      if Used < Header_Bytes then
         return (Success => False, Reason => Truncated);
      elsif Bytes (1 .. 4) /= [16#43#, 16#4C#, 16#4F#, 16#47#] or else
        Bytes (5) /= 1 or else Bytes (6) > Severity'Pos (Severity'Last) or else
        Bytes (7) > Timebase'Pos (Timebase'Last) or else Bytes (8) /= 0 or else
        (for some I in 11 .. 16 => Bytes (I) /= 0)
      then
         return (Success => False, Reason => Invalid_Header);
      end if;
      Length := Natural (Bytes (9)) + Natural (Bytes (10)) * 256;
      if Length > Maximum_Text_Bytes or else Used /= Header_Bytes + Length then
         return (Success => False, Reason => Length_Mismatch);
      end if;
      Ticks := Get_Word (Bytes, 17);
      Domain := Get_Word (Bytes, 25);
      case Timebase'Val (Bytes (7)) is
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
      Item.Length := Length;
      Item.Importance := Severity'Val (Bytes (6));
      for I in 1 .. Item.Length loop
         Item.Content (I) := Character'Val (Bytes (Header_Bytes + I));
      end loop;
      if not Valid_Text (Text (Item)) then
         return (Success => False, Reason => Invalid_Text);
      end if;
      return (Success => True, Value => Item);
   end Decode;
end CuBit.Log_Records;
