with Ada.Command_Line;
with Ada.Text_IO;
with CBOR; use CBOR;
with CBOR.Encoding;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Persistence; use CCL.Objects.Persistence;

procedure Persistence_Tests is
   use type CBOR.Byte;
   use type CBOR.Byte_Array;
   use type CBOR.SE_Offset;
   package E renames CBOR.Encoding;
   Types : Registry;
   Point, Choice, Settings : Type_Reference;
   Def : Definition_Result;
   Contract, Other : Binding;
   Good, Restored : CCL.Objects.Image;
   Wire, Again : Packet;
   Status : Outcome;
   Built : Build_Result;
   Accepted : Boolean;
   Checks : Natural := 0;
   Key : constant Schema_Key :=
     [16#0001020304050607#, 16#08090A0B0C0D0E0F#,
      16#1011121314151617#, 16#18191A1B1C1D1E1F#];
   Schema_Bytes : CBOR.Byte_Array (1 .. 32);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "check" & Checks'Image; end if;
   end Check;
   procedure Add (Value : Cell) is
   begin
      Append (Good, Value, Built); Check (Built = Added);
   end Add;
   procedure Roundtrip is
   begin
      Encode (Good, Contract, Wire, Status); Check (Status = Success);
      Decode (Wire.Data (1 .. SE_Offset (Wire.Length)), Contract, Restored, Status);
      Check (Status = Success and then Restored = Good);
      Encode (Restored, Contract, Again, Status);
      Check (Status = Success and then Again = Wire);
   end Roundtrip;
   procedure Reject (Bytes : Byte_Array) is
   begin
      Restored := Good;
      Decode (Bytes, Contract, Restored, Status);
      Check (Status /= Success and then Restored = Empty (Contract));
   end Reject;
   procedure Write_Hex (Path : String; Data : Byte_Array) is
      File : Ada.Text_IO.File_Type;
      Hex_Digits : constant String := "0123456789abcdef";
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      for B of Data loop
         Ada.Text_IO.Put (File, Hex_Digits (Integer (B) / 16 + 1));
         Ada.Text_IO.Put (File, Hex_Digits (Integer (B) mod 16 + 1));
      end loop;
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Close (File);
   end Write_Hex;
   function Read_Hex (Path : String) return Byte_Array is
      File : Ada.Text_IO.File_Type;
      function Nibble (C : Character) return Byte is
      begin
         case C is
            when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
            when others => raise Program_Error;
         end case;
      end Nibble;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
         Bytes : Byte_Array (1 .. SE_Offset (Line'Length / 2));
      begin
         Check (Line'Length mod 2 = 0);
         for I in Bytes'Range loop
            Bytes (I) := Nibble (Line (Integer (I) * 2 - 1)) * 16 + Nibble (Line (Integer (I) * 2));
         end loop;
         Ada.Text_IO.Close (File);
         return Bytes;
      end;
   end Read_Hex;
begin
   for I in Schema_Bytes'Range loop Schema_Bytes (I) := Byte (I - 1); end loop;
   Define (Types, (Identifier => Named ("Point"), Form => Product, Count => 2,
     Parts => [1 => (Named ("x"), Integer_Type), 2 => (Named ("y"), Integer_Type), others => <>]), Point, Def);
   Check (Def = Defined);
   Define (Types, (Identifier => Named ("Choice"), Form => Sum, Count => 2,
     Parts => [1 => (Named ("Missing"), Unit_Type), 2 => (Named ("At"), Point), others => <>]), Choice, Def);
   Check (Def = Defined);
   Define (Types, (Identifier => Named ("Settings"), Form => Product, Count => 3,
     Parts => [1 => (Named ("title"), String_Type), 2 => (Named ("position"), Choice),
               3 => (Named ("enabled"), Boolean_Type), others => <>]), Settings, Def);
   Check (Def = Defined);
   Bind (Types, Settings, Key, Contract, Accepted); Check (Accepted);
   Good := Empty (Contract);
   Add (Product_Cell (3));
   Append_Text (Good, "Cubie" & Character'Val (0) & Character'Val (255), Built); Check (Built = Added);
   Add (Variant_Cell (2)); Add (Product_Cell (2));
   Add (Integer_Cell (Integer_64'First)); Add (Integer_Cell (Integer_64'Last)); Add (Boolean_Cell (True));
   Roundtrip;
   if Ada.Command_Line.Argument_Count = 2 then
      if Ada.Command_Line.Argument (1) = "--emit" then
         Write_Hex (Ada.Command_Line.Argument (2), Wire.Data (1 .. SE_Offset (Wire.Length)));
      elsif Ada.Command_Line.Argument (1) = "--check" then
         Decode (Read_Hex (Ada.Command_Line.Argument (2)), Contract, Restored, Status);
         Check (Status = Success and then Restored = Good);
      else raise Program_Error;
      end if;
   end if;
   --  Canonical bytes, independently constructed from the documented envelope.
   Check (Wire.Data (1 .. SE_Offset (Wire.Length)) =
     E.Encode_Array (4) & E.Encode_Unsigned (1) & E.Encode_Byte_String (Schema_Bytes) &
     E.Encode_Array (7) & E.Encode_Array (2) & E.Encode_Unsigned (3) & E.Encode_Unsigned (0) &
     E.Encode_Array (2) & E.Encode_Unsigned (0) & E.Encode_Unsigned (7) &
     E.Encode_Array (2) & E.Encode_Unsigned (2) & E.Encode_Unsigned (0) &
     E.Encode_Array (2) & E.Encode_Unsigned (2) & E.Encode_Unsigned (0) &
     E.Encode_Array (2) & E.Encode_Unsigned (2**63) & E.Encode_Unsigned (0) &
     E.Encode_Array (2) & E.Encode_Unsigned (2**63 - 1) & E.Encode_Unsigned (0) &
     E.Encode_Array (2) & E.Encode_Unsigned (1) & E.Encode_Unsigned (0) &
     E.Encode_Byte_String ([67, 117, 98, 105, 101, 0, 255]));
   --  Every single-byte mutation must reject cleanly or be a canonical,
   --  schema-valid alternative value; acceptance must not normalize bad wire.
   declare
      Mutated, Canonical : Packet;
   begin
      for I in 1 .. SE_Offset (Wire.Length) loop
         for B in Byte loop
            Mutated := Wire; Mutated.Data (I) := B;
            Decode (Mutated.Data (1 .. SE_Offset (Mutated.Length)), Contract, Restored, Status);
            if Status = Success then
               Check (Validate (Restored, Contract));
               Encode (Restored, Contract, Canonical, Status);
               Check (Status = Success and Canonical = Mutated);
            else Check (Restored = Empty (Contract));
            end if;
         end loop;
      end loop;
   end;
   for Length in 0 .. Wire.Length - 1 loop Reject (Wire.Data (1 .. SE_Offset (Length))); end loop;
   Reject (Wire.Data (1 .. SE_Offset (Wire.Length)) & Byte_Array'[0]);
   declare
      Shifted : constant Byte_Array (100 .. 99 + SE_Offset (Wire.Length)) := Wire.Data (1 .. SE_Offset (Wire.Length));
      Negative : constant Byte_Array (-SE_Offset (Wire.Length) .. -1) := Wire.Data (1 .. SE_Offset (Wire.Length));
   begin
      Decode (Shifted, Contract, Restored, Status); Check (Status = Success and Restored = Good);
      Reject (Negative);
   end;
   --  Hostile headers, wrong kinds, indefinite containers, and non-shortest integers.
   for I in 1 .. 4 loop
      Again := Wire;
      case I is
         when 1 => Again.Data (1) := 16#9F#;
         when 2 => Again.Data (2) := 2;
         when 3 => Again.Data (3) := 16#78#;
         when others => Again.Data (5) := 99;
      end case;
      Reject (Again.Data (1 .. SE_Offset (Again.Length)));
   end loop;
   Reject (Byte_Array'[16#84#, 16#18#, 1] & Wire.Data (3 .. SE_Offset (Wire.Length)));
   Reject (E.Encode_Array (4) & E.Encode_Unsigned (1) & E.Encode_Byte_String (Schema_Bytes) & E.Encode_Array (257));
   Reject (E.Encode_Array (4) & E.Encode_Unsigned (1) & E.Encode_Byte_String (Schema_Bytes) & E.Encode_Array (0));
   --  Correct schema identity with the wrong trusted type still cannot decode.
   Bind (Types, Boolean_Type, Key, Other, Accepted); Check (Accepted);
   Decode (Wire.Data (1 .. SE_Offset (Wire.Length)), Other, Restored, Status);
   Check (Status = Invalid_Object and Restored = Empty (Other));
   Good.Padding (1) := 1;
   Encode (Good, Contract, Again, Status); Check (Status = Invalid_Object and Again.Length = 0);
   --  Full text arena, including bytes that are not UTF-8.
   Bind (Types, String_Type, Key, Contract, Accepted); Check (Accepted);
   Good := Empty (Contract);
   Append_Text (Good, "", Built); Check (Built = Added); Roundtrip;
   Again := Wire;
   Again.Data (SE_Offset (Again.Length)) := 16#5F#;
   Reject (Again.Data (1 .. SE_Offset (Again.Length)));
   Good := Empty (Contract);
   Append_Text (Good, String'(1 .. Maximum_Text_Bytes => Character'Val (255)), Built);
   Check (Built = Added); Roundtrip;
   --  Maximum product tree exceeds upstream Decode_All's 128-item limit.
   declare
      Leaf, Tree : Type_Reference;
      Parts : Component_Array;
   begin
      for I in Parts'Range loop Parts (I) := (Named ("n" & Character'Val (64 + I)), Integer_Type); end loop;
      Define (Types, (Named ("Leaf"), Product, 14, Parts), Leaf, Def); Check (Def = Defined);
      for I in Parts'Range loop Parts (I).Payload := Leaf; end loop;
      Define (Types, (Named ("Tree"), Product, 16, Parts), Tree, Def); Check (Def = Defined);
      Bind (Types, Tree, Key, Contract, Accepted); Check (Accepted);
      Good := Empty (Contract); Add (Product_Cell (16));
      for I in 1 .. 16 loop
         Add (Product_Cell (14));
         for J in 1 .. 14 loop Add (Integer_Cell (-1)); end loop;
      end loop;
      Roundtrip;
   end;
   Ada.Text_IO.Put_Line ("CCL object CBOR persistence: PASS" & Checks'Image & " checks");
end Persistence_Tests;
