with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Views;
with Config_Read_Outcomes;
with Config_Object_Messages;

procedure View_Tests is
   package V renames CCL.Objects.Views;
   package R renames Config_Read_Outcomes;
   Types, Shifted : Registry;
   Choice, Root_Type, Moved, Ref : Type_Reference;
   Defined_Result : Definition_Result;
   Imported : Import_Result;
   D : CCL.Types.Description;
   Contract, Wrong, Part_Contract : Binding;
   Input, Envelope, Copied, Expected : CCL.Objects.Image;
   Read_Type : R.Description;
   Object : V.Snapshot;
   Root, Payload, Record_Value, Text_Value, Old_Root : V.Cursor;
   Good : Boolean;
   Built : Build_Result;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "CCL object view check" & Checks'Image; end if;
   end Check;
   procedure Append_Cell (C : Cell) is
   begin
      Append (Input, C, Built); Check (Built = Added);
   end Append_Cell;
begin
   Check (not V.Is_Valid (Object, V.Root (Object)));
   Check (V.Type_Of (Object, V.No_Value) = Invalid_Type);
   D := (Identifier => Named ("Reading"), Form => Sum, Count => 2, others => <>);
   D.Parts (1) := (Named ("Text"), String_Type);
   D.Parts (2) := (Named ("Value"), Integer_Type);
   Define (Types, D, Choice, Defined_Result); Check (Defined_Result = Defined);
   D := (Identifier => Named ("Settings"), Form => Product, Count => 3, others => <>);
   D.Parts (1) := (Named ("reading"), Choice);
   D.Parts (2) := (Named ("title"), String_Type);
   D.Parts (3) := (Named ("enabled"), Boolean_Type);
   Define (Types, D, Root_Type, Defined_Result); Check (Defined_Result = Defined);
   Bind (Types, Root_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   R.Define (Contract, Named ("Snapshot"), Named ("Read"), [5, 6, 7, 8], Read_Type, Good); Check (Good);
   for Is_Text in Boolean loop
      for Empty_Text in Boolean loop
         Input := Empty (Contract);
         Append_Cell (Product_Cell (3));
         Append_Cell (Variant_Cell (if Is_Text then 1 else 2));
         if Is_Text then
            Append_Text (Input, (if Empty_Text then "" else "hello"), Built); Check (Built = Added);
         else Append_Cell (Integer_Cell (Integer_64'First));
         end if;
         Append_Text (Input, "title", Built); Check (Built = Added);
         Append_Cell (Boolean_Cell (True));
         R.Build (Read_Type, True, Config_Object_Messages.Success, 42, Input, Envelope, Good); Check (Good);
         V.Capture (Object, R.Schema (Read_Type), Envelope, Good); Check (Good);
         Root := V.Root (Object);
         Check (V.Alternative (Object, Root) = R.Alternative'Enum_Rep (R.Found));
         Payload := V.Payload (Object, Root);
         Check (V.Describe (Object, Payload).Form = Product);
         Check (Integer_Of (V.Scalar (Object, V.Field (Object, Payload, 1))) = 42);
         Record_Value := V.Field (Object, Payload, 2);
         V.Copy_Value (Object, Record_Value, Contract, Copied, Good);
         Check (Good and Copied = Input);
         V.Copy_Value (Object, Root, R.Schema (Read_Type), Copied, Good);
         Check (Good and Copied = Envelope);
         -- Extract a later text field: preceding text must not escape, and
         -- its old nonzero offset must become zero in the new native image.
         Bind (Types, String_Type, [31, 32, 33, 34], Part_Contract, Good); Check (Good);
         V.Copy_Value (Object, V.Field (Object, Record_Value, 2), Part_Contract, Copied, Good);
         Expected := Empty (Part_Contract);
         Append_Text (Expected, "title", Built); Check (Built = Added);
         Check (Good and Copied = Expected);
         V.Copy_Value (Object, Record_Value, Part_Contract, Copied, Good);
         Check (not Good and Copied = Empty (Part_Contract));
         Check (Same (V.Describe (Object, Record_Value).Identifier, Named ("Settings")));
         Check (V.Alternative (Object, V.Field (Object, Record_Value, 1)) = (if Is_Text then 1 else 2));
         Text_Value := V.Payload (Object, V.Field (Object, Record_Value, 1));
         if Is_Text then
            Check (V.Text (Object, Text_Value) = (if Empty_Text then "" else "hello"));
         else Check (Integer_Of (V.Scalar (Object, Text_Value)) = Integer_64'First);
         end if;
         Check (V.Text (Object, V.Field (Object, Record_Value, 2)) = "title");
         Check (V.Scalar (Object, V.Field (Object, Record_Value, 3)) = Boolean_Cell (True));
         Check (not V.Is_Valid (Object, V.Field (Object, Record_Value, 4)));
         Check (not V.Is_Valid (Object, V.Field (Object, Root, 1)));
         Check (not V.Is_Valid (Object, V.Payload (Object, Record_Value)));
         -- Snapshot does not refer back into its source buffer.
         Envelope := (others => <>);
         Check (V.Text (Object, V.Field (Object, Record_Value, 2)) = "title");
         Old_Root := Root;
         V.Clear (Object); Check (not V.Is_Valid (Object, Old_Root));
         V.Copy_Value (Object, Old_Root, Contract, Copied, Good);
         Check (not Good and Copied = Empty (Contract));
         V.Capture (Object, Contract, Input, Good); Check (Good);
         Check (not V.Is_Valid (Object, Old_Root));
      end loop;
   end loop;
   Define (Shifted, (Identifier => Named ("Before"), Form => Product, others => <>), Ref, Defined_Result);
   Check (Defined_Result = Defined);
   Import_Definition (Types, Root_Type, Shifted, Moved, Imported); Check (Imported = CCL.Types.Imported);
   Check (Moved /= Root_Type and V.Local_Type (Object, V.Root (Object), Shifted) = Moved);
   Bind (Shifted, Moved, Identity (Contract), Part_Contract, Good); Check (Good);
   V.Copy_Value (Object, V.Root (Object), Part_Contract, Copied, Good);
   Check (Good and Copied = Input);
   -- Same layout, different nominal name is not the same type.
   D.Identifier := Named ("OtherSettings");
   Define (Types, D, Ref, Defined_Result); Check (Defined_Result = Defined);
   Bind (Types, Ref, Identity (Contract), Part_Contract, Good); Check (Good);
   V.Copy_Value (Object, V.Root (Object), Part_Contract, Copied, Good);
   Check (not Good and Copied = Empty (Part_Contract));
   V.Copy_Value (Object, V.No_Value, Contract, Copied, Good);
   Check (not Good and Copied = Empty (Contract));
   Check (V.Local_Type (Object, V.Field (Object, V.Root (Object), 2), Shifted) = String_Type);
   -- A different key or malformed data cannot replace a valid snapshot while
   -- leaving its old cursor live, and no partial index can escape a failure.
   Old_Root := V.Root (Object);
   Bind (Types, Root_Type, [9, 9, 9, 9], Wrong, Good); Check (Good);
   V.Capture (Object, Wrong, Input, Good); Check (not Good);
   Check (not V.Is_Valid (Object, Old_Root) and not V.Is_Valid (Object, V.Root (Object)));
   for Fault in 1 .. 6 loop
      Envelope := Input;
      case Fault is
         when 1 => Envelope.Used_Cells := 0;
         when 2 => Envelope.Used_Cells := Unsigned_32'Last;
         when 3 => Envelope.Cells (2).First := Unsigned_64'Last;
         when 4 => Envelope.Cells (1).First := 2;
         when 5 => Envelope.Padding (1) := 1;
         when others => Envelope.Reserved := 1;
      end case;
      V.Capture (Object, Contract, Envelope, Good); Check (not Good);
      Check (not V.Is_Valid (Object, V.Root (Object)));
   end loop;
   -- Full text and canonical empty string bounds.
   Bind (Types, String_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Input := Empty (Contract);
   Append_Text (Input, String'(1 .. Maximum_Text_Bytes => 'x'), Built); Check (Built = Added);
   V.Capture (Object, Contract, Input, Good); Check (Good);
   Check (V.Text (Object, V.Root (Object)) = String'(1 .. Maximum_Text_Bytes => 'x'));
   declare
      Ch : Character;
      Full : String (17 .. 16 + Maximum_Text_Bytes);
      Short : String (1 .. 3);
   begin
      Check (V.Text_Length (Object, V.Root (Object)) = Maximum_Text_Bytes);
      V.Read_Text (Object, V.Root (Object), 1, Ch, Good); Check (Good and Ch = 'x');
      V.Read_Text (Object, V.Root (Object), Maximum_Text_Bytes, Ch, Good); Check (Good and Ch = 'x');
      V.Read_Text (Object, V.Root (Object), Maximum_Text_Bytes + 1, Ch, Good);
      Check (not Good and Ch = Character'Val (0));
      V.Copy_Text (Object, V.Root (Object), Full, Good);
      Check (Good and Full = String'(1 .. Maximum_Text_Bytes => 'x'));
      V.Copy_Text (Object, V.Root (Object), Short, Good);
      Check (not Good and Short = String'(1 .. 3 => Character'Val (0)));
      V.Read_Text (Object, V.No_Value, 1, Ch, Good); Check (not Good);
      V.Copy_Text (Object, V.No_Value, Short, Good); Check (not Good);
      Check (V.Text_Length (Object, V.No_Value) = 0);
   end;
   V.Copy_Value (Object, V.Root (Object), Contract, Copied, Good);
   Check (Good and Copied = Input);
   Input := Empty (Contract); Append_Text (Input, "", Built); Check (Built = Added);
   V.Capture (Object, Contract, Input, Good); Check (Good);
   Check (V.Text (Object, V.Root (Object)) = "");
   declare
      Ch : Character;
      Nothing : String (4 .. 3);
   begin
      Check (V.Text_Length (Object, V.Root (Object)) = 0);
      V.Copy_Text (Object, V.Root (Object), Nothing, Good); Check (Good);
      V.Read_Text (Object, V.Root (Object), 1, Ch, Good); Check (not Good);
      Old_Root := V.Root (Object);
      V.Clear (Object);
      V.Copy_Text (Object, Old_Root, Nothing, Good); Check (not Good);
      V.Read_Text (Object, Old_Root, 1, Ch, Good); Check (not Good);
      V.Capture (Object, Contract, Input, Good); Check (Good);
   end;
   V.Copy_Value (Object, V.Root (Object), Contract, Copied, Good);
   Check (Good and Copied = Input);
   -- Local constructors do not mint an advertised schema identity.
   Expected := Input; Expected.Schema := No_Schema;
   V.Capture_Local (Object, Types, String_Type, Expected, Good); Check (Good);
   V.Copy_Value (Object, V.Root (Object), Contract, Copied, Good);
   Check (Good and Copied = Input);
   Bind (Types, String_Type, No_Schema, Wrong, Good); Check (not Good);
   V.Copy_Value (Object, V.Root (Object), Wrong, Copied, Good); Check (not Good);
   V.Capture_Local (Object, Types, String_Type, Input, Good); Check (not Good);
   V.Capture_Local (Object, Types, Handler_Type, Expected, Good); Check (not Good);
   Ada.Text_IO.Put_Line ("Owned CCL object views: PASS" & Checks'Image & " checks");
end View_Tests;
