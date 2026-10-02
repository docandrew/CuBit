pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Log_Records; use CuBit.Log_Records;

--  Linux-hosted regression tests for LogRecord v2 structured fields.
procedure Main is
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   Base : constant Decoded := Make ("frame presented", Warning,
                                   (Monotonic_Milliseconds, 7, 1234));
   Item : Decoded;
   Bytes, Bad : Wire_Buffer;
   Used : Wire_Count;
   Again : Decoded;
   Names : constant array (Field_Index) of String (1 .. 4) :=
     ["f.00", "f-01", "f_02", "f.03", "f.04", "f.05", "f.06", "f.07"];
   Field_Offset : constant := Header_Bytes;
   Kind_Offset : constant := 17;
   Value_Offset : constant := 25;
   Version_Byte : constant := 5;
   Count_Byte : constant := 11;
   Truth_Code : constant := 4;

   function Add
     (Value : Decoded; Name : String; Kind : Field_Kind; Raw : Unsigned_64)
      return Decoded is
     (if Value.Success then With_Field (Value.Value, Name, Kind, Raw)
      else Value);
   procedure Refuse_Name (Bad_Name : String) is
      Result : constant Decoded :=
        With_Field (Base.Value, Bad_Name, Unsigned_Integer, 0);
   begin
      Check (not Result.Success and then Result.Reason = Invalid_Field,
             "bad name refused: " & Bad_Name);
   end Refuse_Name;
begin
   Check (Base.Success, "base record");

   --  API: every kind, accessors and signed two's complement.
   Item := Add (Base, "latency_us", Duration_Microseconds, 4_167);
   Item := Add (Item, "delta", Signed_Integer, Unsigned_64'Last);
   Item := Add (Item, "minimum", Signed_Integer, 2 ** 63);
   Item := Add (Item, "frame", Unsigned_Integer, Unsigned_64'Last);
   Item := Add (Item, "late", Truth, 1);
   Check (Item.Success and then Field_Total (Item.Value) = 5, "five fields");
   if Item.Success then
      Check (Name (Field_At (Item.Value, 1)) = "latency_us" and
             Kind (Field_At (Item.Value, 1)) = Duration_Microseconds and
             Value (Field_At (Item.Value, 1)) = 4_167, "first field");
      Check (Signed_Value (Field_At (Item.Value, 2)) = -1, "signed -1");
      Check (Signed_Value (Field_At (Item.Value, 3)) = Integer_64'First,
             "signed minimum");
      Check (Signed_Value (Field_At (Item.Value, 4)) = -1 and
             Value (Field_At (Item.Value, 4)) = Unsigned_64'Last,
             "raw unsigned");
      Check (Text (Item.Value) = "frame presented" and
             Level (Item.Value) = Warning, "text/level kept");
      Encode (Item.Value, Bytes, Used);
      Check (Used = Header_Bytes + 5 * Field_Bytes + 15, "v2 length");
      Check (Bytes (Version_Byte) = 2 and Bytes (Count_Byte) = 5, "v2 hdr");
      --  Independent field vector: name, kind code, little-endian value.
      Check (Bytes (Field_Offset + 1) = Character'Pos ('l') and
             Bytes (Field_Offset + 10) = Character'Pos ('s') and
             Bytes (Field_Offset + 11) = 0 and
             Bytes (Field_Offset + Kind_Offset) = 3 and
             Bytes (Field_Offset + Value_Offset) = 16#47# and
             Bytes (Field_Offset + Value_Offset + 1) = 16#10#,
             "field wire vector");
      Again := Decode (Bytes, Used);
      Check (Again.Success and then Again.Value = Item.Value, "v2 trip");
      for Length in Wire_Count loop
         Check (Decode (Bytes, Length).Success = (Length = Used),
                "only exact length decodes");
      end loop;
   end if;

   --  Records without fields stay byte-identical version 1.
   Encode (Base.Value, Bytes, Used);
   Check (Bytes (Version_Byte) = 1 and Bytes (Count_Byte) = 0 and
          Used = Header_Bytes + 15, "no fields -> v1");

   --  Construction limits.
   Item := Base;
   for F in Field_Index loop
      Item := Add (Item, Names (F), Unsigned_Integer, Unsigned_64 (F));
   end loop;
   Check (Item.Success and then Field_Total (Item.Value) = Maximum_Fields,
          "eight fields");
   if Item.Success then
      Again := With_Field (Item.Value, "x", Truth, 0);
      Check (not Again.Success and then Again.Reason = Too_Many_Fields,
             "ninth field refused");
      Encode (Item.Value, Bytes, Used);
      Again := Decode (Bytes, Used);
      Check (Again.Success and then Again.Value = Item.Value, "eight trip");
   end if;
   Refuse_Name ("");
   Refuse_Name ("Upper");
   Refuse_Name ("sp ace");
   Refuse_Name ("0123456789abcdefg");
   Refuse_Name ("tab" & ASCII.HT);
   Check (With_Field (Base.Value, "0123456789abcdef", Truth, 0).Success,
          "16-byte name allowed");
   Again := With_Field (Base.Value, "flag", Truth, 2);
   Check (not Again.Success and then Again.Reason = Invalid_Field,
          "truth must be 0/1");

   --  Wire rejection.
   Item := Add (Base, "flag", Truth, 1);
   Encode (Item.Value, Bytes, Used);
   Bad := Bytes; Bad (Count_Byte) := 0;
   Check (not Decode (Bad, Used).Success, "v2 with zero fields");
   Bad := Bytes; Bad (Version_Byte) := 1;
   Check (not Decode (Bad, Used).Success, "v1 with fields");
   Bad := Bytes; Bad (Version_Byte) := 3;
   Check (not Decode (Bad, Used).Success, "unknown version");
   Bad := Bytes; Bad (Count_Byte) := 9;
   Check (not Decode (Bad, Wire_Count'Last).Success, "nine fields");
   Bad := Bytes; Bad (Field_Offset + 2) := 0;
   Check (not Decode (Bad, Used).Success, "embedded zero in name");
   Bad := Bytes; Bad (Field_Offset + 1) := 0;
   Check (not Decode (Bad, Used).Success, "empty name");
   Bad := Bytes; Bad (Field_Offset + 1) := Character'Pos ('F');
   Check (not Decode (Bad, Used).Success, "uppercase name");
   for Code in Unsigned_8'(0) .. 6 loop
      Bad := Bytes; Bad (Field_Offset + Kind_Offset) := Code;
      Bad (Field_Offset + Value_Offset) := 0;
      Check (Decode (Bad, Used).Success = (Code in 1 .. 4), "kind codes");
   end loop;
   for Offset in 18 .. 24 loop
      Bad := Bytes; Bad (Field_Offset + Offset) := 1;
      Check (not Decode (Bad, Used).Success, "field reserved byte");
   end loop;
   Bad := Bytes; Bad (Field_Offset + Value_Offset) := 2;
   Check (Bad (Field_Offset + Kind_Offset) = Truth_Code and then
          not Decode (Bad, Used).Success, "truth value 2 on wire");

   --  Canonical encoding: random single-byte corruptions either fail or
   --  decode to a record whose encoding reproduces the exact bytes.
   declare
      Seed : Unsigned_64 := 16#2545_F491_4F6C_DD1D#;
      function Next return Unsigned_64 is
      begin
         Seed := Seed xor Shift_Left (Seed, 13);
         Seed := Seed xor Shift_Right (Seed, 7);
         Seed := Seed xor Shift_Left (Seed, 17);
         return Seed;
      end Next;
      Reencoded : Wire_Buffer;
      Reused : Wire_Count;
      Accepted : Natural := 0;
   begin
      Item := Add (Base, "latency_us", Duration_Microseconds, 4_167);
      Item := Add (Item, "late", Truth, 0);
      Encode (Item.Value, Bytes, Used);
      for Trial in 1 .. 100_000 loop
         Bad := Bytes;
         Bad (Natural (Next mod Unsigned_64 (Used)) + 1) :=
           Unsigned_8 (Next mod 256);
         Again := Decode (Bad, Used);
         if Again.Success then
            Accepted := Accepted + 1;
            Encode (Again.Value, Reencoded, Reused);
            Check (Reused = Used and then Reencoded = Bad, "canonical");
         end if;
      end loop;
      Check (Accepted > 1_000, "fuzz accepted enough");
   end;

   Put_Line ("log-fields:" & Natural'Image (Checks) & " checks," &
             Natural'Image (Failures) & " failures");
   if Failures /= 0 then
      raise Program_Error with "log-fields tests failed";
   end if;
end Main;
