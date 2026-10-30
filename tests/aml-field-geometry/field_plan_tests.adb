pragma Ada_2022;
with Ada.Text_IO;
with Field_Datum_Tests;
with AML_Decode;
with AML_Field_Datums;
with AML_Field_Layout;
with AML_Field_Plans;
procedure Field_Plan_Tests is
   package D renames AML_Field_Datums;
   package L renames AML_Field_Layout;
   package P renames AML_Field_Plans;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   use type D.Update_Rule;
   use type P.Datum_Count;
   use type P.Read_Form;
   type Shape is record
      Access_Code : AML_Decode.Byte;
      Offset, Length : AML_Decode.Integer_Value;
      Datums : P.Datum_Count;
      Buffer_Size : Positive;
      Narrow_Form : P.Read_Form;
   end record;
   -- Shapes and result kinds observed in the normal ACPICA partial corpus.
   Shapes : constant array (Positive range 1 .. 5) of Shape :=
     [(1, 3, 5, 1, 1, P.Integer_Result),
      (1, 7, 12, 3, 2, P.Integer_Result),
      (2, 11, 23, 3, 3, P.Integer_Result),
      (3, 27, 37, 2, 5, P.Buffer_Result),
      (0, 7, 64, 9, 8, P.Buffer_Result)];
   Qword_Shapes : constant array (Positive range 1 .. 4) of Shape :=
     [(4, 0, 64, 1, 8, P.Buffer_Result),
      (4, 7, 64, 2, 8, P.Buffer_Result),
      (4, 63, 64, 2, 8, P.Buffer_Result),
      (4, 27, 37, 1, 5, P.Buffer_Result)];
   Update_Stride : constant := 16#20#;
   Expected_Checks : constant Natural := 686;
   Checks : Natural := 0;
   Field : L.Field_Geometry;
   Item : P.Plan;
   Register : L.Register_Geometry;

   procedure Check (Value : Boolean) is
   begin
      if not Value then raise Program_Error with "field plan check"; end if;
      Checks := Checks + 1;
   end Check;
   function Baseline return L.Field_Geometry is
     (Region => (Space => L.Memory_Space, Base => 16#4000#, Length => 32),
      Bit_Offset => 3, Bit_Length => 5, Flags => 1, Effective_Access => 1,
      others => <>);
   procedure Reject (Value : L.Field_Geometry) is
      Invalid : constant P.Plan := P.Prepare (Value);
   begin
      Check (not P.Valid (Invalid));
      Check (P.Count (Invalid) = 0);
   end Reject;
begin
   Field_Datum_Tests;
   for Space in L.Memory_Space .. L.IO_Space loop
      for Rule in D.Update_Rule loop
         for S of Shapes loop
            Field := Baseline;
            Field.Region.Space := Space;
            Field.Bit_Offset := S.Offset;
            Field.Bit_Length := S.Length;
            Field.Effective_Access := S.Access_Code;
            Field.Flags := S.Access_Code + AML_Decode.Byte (D.Update_Rule'Pos (Rule) * Update_Stride);
            Item := P.Prepare (Field);
            Check (P.Valid (Item));
            Check (P.Count (Item) = S.Datums);
            Check (P.Rule (Item) = Rule);
            Check (P.Buffer_Length (Item) = S.Buffer_Size);
            Check (P.Result_Form (Item, AML_Decode.Bits_32) = S.Narrow_Form);
            Check (P.Result_Form (Item, AML_Decode.Bits_64) = P.Integer_Result);
            for Position in P.Datum_Position range 1 .. P.Count (Item) loop
               declare
                  Datum : constant D.Datum := P.Datum_At (Item, Position);
               begin
                  Register := (Region => Field.Region,
                     Byte_Offset => AML_Decode.Integer_Value (Datum.Offset),
                     Width => Datum.Access_Width);
                  Check (P.Matches (Item, Position, Register));
                  Register.Byte_Offset := Register.Byte_Offset + 1;
                  Check (not P.Matches (Item, Position, Register));
                  Register.Byte_Offset := AML_Decode.Integer_Value (Datum.Offset);
                  Register.Region.Base := Register.Region.Base + 1;
                  Check (not P.Matches (Item, Position, Register));
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Field := (others => <>); Reject (Field);
   Field := Baseline; Field.Bit_Length := 0; Reject (Field);
   Field := Baseline; Field.Bit_Length := 65; Reject (Field);
   Field := Baseline; Field.Bit_Offset := AML_Decode.Integer_Value'Last; Reject (Field);
   Field := Baseline; Field.Region.Base := AML_Decode.Integer_Value'Last; Reject (Field);
   Field := Baseline; Field.Region.Length := 1; Field.Bit_Offset := 7;
   Field.Bit_Length := 2; Reject (Field);
   Field := Baseline; Field.Effective_Access := 2;
   Field.Region.Base := 16#4001#; Reject (Field);
   Field := Baseline; Field.Effective_Access := 3;
   Field.Region.Length := 3; Reject (Field);
   Field := Baseline; Field.Region.Space := 2; Reject (Field);
   Field := Baseline; Field.Form := L.Bank_Field; Reject (Field);
   Field := Baseline; Field.Form := L.Index_Field; Reject (Field);
   Field := Baseline; Field.Has_Connection := True; Reject (Field);
   Field := Baseline; Field.Flags := 16#80#; Reject (Field);
   Field := Baseline; Field.Flags := 16#10#; Reject (Field);
   Field := Baseline; Field.Flags := 16#60#; Reject (Field);
   Field := Baseline; Field.Flags := 6; Reject (Field);
   Field := Baseline; Field.Effective_Access := 4;
   Field.Region.Space := L.IO_Space; Reject (Field);
   Field := Baseline; Field.Effective_Access := 5; Reject (Field);
   Field := Baseline; Field.Attribute := 1; Reject (Field);
   Field := Baseline; Field.Access_Length := 1; Reject (Field);
   Field := Baseline; Field.Region.Space := L.IO_Space;
   Field.Region.Base := 16#10000#; Reject (Field);
   Field := Baseline; Field.Region.Space := L.IO_Space;
   Field.Region.Base := 16#FFFF#; Field.Region.Length := 2; Reject (Field);
   Field := Baseline; Field.Region := (L.IO_Space, 16#FFFF#, 1);
   Field.Bit_Offset := 0; Field.Bit_Length := 8; Item := P.Prepare (Field);
   Check (P.Valid (Item)); Check (P.Count (Item) = 1);
   Field.Region := (L.IO_Space, 0, 16#10000#);
   Field.Bit_Offset := 16#FFFF# * L.Bits_Per_Byte; Item := P.Prepare (Field);
   Check (P.Valid (Item)); Check (P.Count (Item) = 1);
   Field.Region := (L.Memory_Space, AML_Decode.Integer_Value'Last, 1);
   Field.Bit_Offset := 0; Item := P.Prepare (Field);
   Check (P.Valid (Item)); Check (P.Count (Item) = 1);
   Check (not P.Matches (Item, 2, Register));
   Item := P.Prepare ((others => <>));
   Check (not P.Matches (Item, 1, Register));
   for Rule in D.Update_Rule loop
      for S of Qword_Shapes loop
            Field := Baseline;
            Field.Bit_Offset := S.Offset;
            Field.Bit_Length := S.Length;
            Field.Effective_Access := 4;
            Field.Flags := 4 + AML_Decode.Byte (D.Update_Rule'Pos (Rule) * Update_Stride);
            Item := P.Prepare (Field);
            Check (P.Valid (Item));
            Check (P.Count (Item) = S.Datums);
            Check (P.Rule (Item) = Rule);
            Check (P.Buffer_Length (Item) = S.Buffer_Size);
            Check (P.Result_Form (Item, AML_Decode.Bits_32) = P.Buffer_Result);
            Check (P.Result_Form (Item, AML_Decode.Bits_64) = P.Integer_Result);
            for Position in P.Datum_Position range 1 .. P.Count (Item) loop
               declare
                  Datum : constant D.Datum := P.Datum_At (Item, Position);
               begin
                  Register := (Field.Region, AML_Decode.Integer_Value (Datum.Offset), Datum.Access_Width);
                  Check (P.Matches (Item, Position, Register));
                  Register.Byte_Offset := Register.Byte_Offset + 1;
                  Check (not P.Matches (Item, Position, Register));
                  Register.Byte_Offset := AML_Decode.Integer_Value (Datum.Offset);
                  Register.Region.Base := Register.Region.Base + 1;
                  Check (not P.Matches (Item, Position, Register));
               end;
            end loop;
      end loop;
   end loop;
   Field := Baseline; Field.Effective_Access := 4;
   Field.Region.Base := 16#4001#; Reject (Field);
   Field := Baseline; Field.Effective_Access := 4;
   Field.Bit_Offset := 7; Field.Bit_Length := 64; Field.Region.Length := 15; Reject (Field);
   if Checks /= Expected_Checks then raise Program_Error with "field plan count"; end if;
   Ada.Text_IO.Put_Line ("Field plan checks" & Natural'Image (Checks));
end Field_Plan_Tests;
