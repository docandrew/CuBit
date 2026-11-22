with Storehouse.Canonical;
with Storehouse.Hashing;

package body Build_Encoding is
   use Interfaces;
   use Storehouse.Canonical;

   procedure Append (W : in out Writer; Data : Byte_Array) is
   begin
      for B of Data loop
         W.Data.Append (B);
      end loop;
   end Append;

   procedure Add_Unsigned (W : in out Writer; Value : Unsigned_64) is
   begin
      Append (W, Head (Unsigned_Integer, Value));
   end Add_Unsigned;

   procedure Add_Text (W : in out Writer; Text : String) is
   begin
      Append (W, Head (Text_String, Unsigned_64 (Text'Length)));
      for C of Text loop
         W.Data.Append (Character'Pos (C));
      end loop;
   end Add_Text;

   procedure Add_Bytes (W : in out Writer; Data : Byte_Array) is
   begin
      Append (W, Head (Byte_String, Unsigned_64 (Data'Length)));
      Append (W, Data);
   end Add_Bytes;

   procedure Add_Digest (W : in out Writer; Value : Digest) is
   begin
      Add_Bytes (W, Byte_Array (Value));
   end Add_Digest;

   procedure Add_Boolean (W : in out Writer; Value : Boolean) is
   begin
      Add_Unsigned (W, Boolean'Pos (Value));
   end Add_Boolean;

   procedure Add_Array (W : in out Writer; Count : Natural) is
   begin
      Append (W, Head (Array_Of, Unsigned_64 (Count)));
   end Add_Array;

   function Bytes (W : Writer) return Byte_Array is
      Result : Byte_Array (1 .. Natural (W.Data.Length));
   begin
      for I in Result'Range loop
         Result (I) := W.Data (I);
      end loop;
      return Result;
   end Bytes;

   function Identity (W : Writer) return Digest is (Storehouse.Hashing.Of_Bytes (Bytes (W)));

   function Read (R : in out Reader; Expected : Major_Type) return Unsigned_64 is
      Major : Major_Type;
      Value : Unsigned_64;
      Next : Positive;
      Valid : Boolean;
   begin
      Read_Head (R.Data, R.Position, Major, Value, Next, Valid);
      if not Valid or else Major /= Expected then
         raise Decode_Error with "not a canonical " & Expected'Image & " at byte" & R.Position'Image;
      end if;
      R.Position := Next;
      return Value;
   end Read;

   --  A string's length, checked against what remains.
   function Read_Length (R : in out Reader; Expected : Major_Type) return Natural is
      Value : constant Unsigned_64 := Read (R, Expected);
   begin
      if Value > Unsigned_64 (R.Length - R.Position + 1) then
         raise Decode_Error with "string runs past the end";
      end if;
      return Natural (Value);
   end Read_Length;

   function Read_Unsigned (R : in out Reader) return Unsigned_64 is (Read (R, Unsigned_Integer));

   function Read_Text (R : in out Reader) return String is
      Length : constant Natural := Read_Length (R, Text_String);
      Result : String (1 .. Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (R.Data (R.Position + I - 1));
      end loop;
      R.Position := R.Position + Length;
      return Result;
   end Read_Text;

   function Read_Digest (R : in out Reader) return Digest is
      Length : constant Natural := Read_Length (R, Byte_String);
      Result : Digest;
   begin
      if Length /= Digest_Bytes then
         raise Decode_Error with "an identity is" & Digest_Bytes'Image & " bytes";
      end if;
      for I in Digest_Index loop
         Result (I) := R.Data (R.Position + I - 1);
      end loop;
      R.Position := R.Position + Length;
      return Result;
   end Read_Digest;

   function Read_Array (R : in out Reader) return Natural is
      Value : constant Unsigned_64 := Read (R, Array_Of);
   begin
      --  Every item takes at least one byte.
      if Value > Unsigned_64 (R.Length - R.Position + 1) then
         raise Decode_Error with "array runs past the end";
      end if;
      return Natural (Value);
   end Read_Array;

end Build_Encoding;
