with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with CBOR;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CCL.Objects.Schemas.Persistence;
with CCL.Objects.Persistence;

procedure Schema_Codec_Tests is
   package T renames CCL.Types;
   package O renames CCL.Objects;
   package P renames CCL.Objects.Schemas.Persistence;
   use type O.Binding;
   use type T.Type_Reference;
   use type T.Definition_Result;
   use type P.Outcome;
   use type P.Packet;
   use type O.Build_Result;
   use type O.Image;
   use type O.Persistence.Outcome;
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_IO.Count;
   use type CBOR.SE_Offset;
   use type CBOR.Byte;
   use type CBOR.Byte_Array;
   Registry : T.Registry;
   Contract, Restored, Empty : O.Binding;
   Data, Encoded : P.Packet;
   Result : P.Outcome;
   Accepted : Boolean;
   Count : Natural := 0;
   Key : constant O.Schema_Key := [1, 2, 3, 4];
   Golden : CBOR.Byte_Array (1 .. 38) := [others => 0];
   procedure Check (Good : Boolean) is
   begin
      Count := Count + 1;
      if not Good then raise Program_Error with "schema codec check" & Count'Image; end if;
   end Check;
   procedure Reject (Bytes : CBOR.Byte_Array) is
   begin
      Restored := Contract;
      P.Decode (Bytes, Restored, Result);
      Check (Result = P.Invalid_Encoding and Restored = Empty);
   end Reject;
   procedure Roundtrip is
   begin
      P.Encode (Contract, Data, Result);
      Check (Result = P.Success and Data.Length > 0);
      P.Decode (Data.Data (1 .. CBOR.SE_Offset (Data.Length)), Restored, Result);
      Check (Result = P.Success and Restored = Contract);
      P.Encode (Restored, Encoded, Result);
      Check (Result = P.Success and Encoded = Data);
   end Roundtrip;
   procedure Write_Bytes (Path : String; Bytes : CBOR.Byte_Array) is
      File : Ada.Streams.Stream_IO.File_Type;
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Bytes'Length));
   begin
      for I in Raw'Range loop Raw (I) := Ada.Streams.Stream_Element (Bytes (CBOR.SE_Offset (I))); end loop;
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Path);
      Ada.Streams.Stream_IO.Write (File, Raw);
      Ada.Streams.Stream_IO.Close (File);
   end Write_Bytes;
   function Read_Bytes (Path : String) return CBOR.Byte_Array is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);
      Check (Ada.Streams.Stream_IO.Size (File) <= P.Maximum_Encoded_Bytes);
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Ada.Streams.Stream_IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Bytes : CBOR.Byte_Array (1 .. CBOR.SE_Offset (Raw'Length));
      begin
         Ada.Streams.Stream_IO.Read (File, Raw, Last);
         Ada.Streams.Stream_IO.Close (File);
         Check (Last = Raw'Last);
         for I in Raw'Range loop Bytes (CBOR.SE_Offset (I)) := CBOR.Byte (Raw (I)); end loop;
         return Bytes;
      end;
   end Read_Bytes;
begin
   P.Encode (Empty, Data, Result);
   Check (Result = P.Invalid_Binding and Data.Length = 0);
   O.Bind (Registry, T.Integer_Type, Key, Contract, Accepted);
   Check (Accepted);
   Roundtrip;
   Golden (1 .. 4) := [16#84#, 1, 16#58#, 32];
   Golden (12) := 1; Golden (20) := 2; Golden (28) := 3; Golden (36) := 4;
   Golden (37) := 1; Golden (38) := 16#80#;
   Check (Data.Length = 38 and Data.Data (1 .. 38) = Golden);
   for Last in CBOR.SE_Offset range 0 .. Golden'Last - 1 loop Reject (Golden (1 .. Last)); end loop;
   Reject (Golden & [0]);
   Reject ([16#98#, 4] & Golden (2 .. 38)); -- overlong outer array
   Reject (Golden (1 .. 1) & [16#18#, 1] & Golden (3 .. 38)); -- overlong version
   Reject (Golden (1 .. 2) & [16#59#, 0, 32] & Golden (5 .. 38));
   Reject (Golden (1 .. 36) & [16#18#, 1, 16#80#]);
   Reject (Golden (1 .. 37) & [16#98#, 0]);
   Reject (Golden (1 .. 36) & [0, 16#80#]); -- invalid root
   Reject (Golden (1 .. 36) & [5, 16#80#]); -- live handler is not data
   Reject (Golden (1 .. 36) & [7, 16#80#]); -- missing declaration
   Reject (Golden (1 .. 36) & [1, 16#9F#, 16#FF#]); -- indefinite definitions
   Reject (Golden (1 .. 36) & [1, 16#98#, 33]); -- excessive count
   declare
      Shifted : CBOR.Byte_Array (101 .. 138) := Golden;
      Negative : CBOR.Byte_Array (-38 .. -1) := Golden;
   begin
      P.Decode (Shifted, Restored, Result);
      Check (Result = P.Success and Restored = Contract);
      Reject (Negative);
   end;
   --  Every single-byte mutation either rejects completely or has an exact,
   --  canonical re-encoding. This is parser fuzzing, not a soundness proof.
   for Position in Golden'Range loop
      for Byte in CBOR.Byte loop
         declare
            Changed : CBOR.Byte_Array := Golden;
         begin
            Changed (Position) := Byte;
            P.Decode (Changed, Restored, Result);
            if Result = P.Success then
               P.Encode (Restored, Encoded, Result);
               Check (Result = P.Success and then Encoded.Length = Changed'Length and then
                 Encoded.Data (1 .. CBOR.SE_Offset (Encoded.Length)) = Changed);
            else Check (Restored = Empty);
            end if;
         end;
      end loop;
   end loop;
   -- A discoverable resource is never disk data. Its mere visibility must
   -- not change the portable representation of an unrelated data root.
   declare
      Types : T.Registry;
      D : T.Description;
      Ref, Settings : T.Type_Reference;
      Defined : T.Definition_Result;
      Clean, Visible : O.Binding;
      Expected, Actual : P.Packet;
   begin
      D := (Identifier => T.Named ("Settings"), Form => T.Product, Count => 1,
            Parts => [1 => (T.Named ("caption"), T.String_Type), others => <>]);
      T.Define (Types, D, Settings, Defined); Check (Defined = T.Defined);
      O.Bind (Types, Settings, Key, Clean, Accepted); Check (Accepted);
      P.Encode (Clean, Expected, Result); Check (Result = P.Success);
      D := (Identifier => T.Named ("Collection"), Form => T.Resource, Count => 1,
            Parts => [1 => (T.Named ("Value"), Settings), others => <>]);
      T.Define (Types, D, Ref, Defined); Check (Defined = T.Defined);
      O.Bind (Types, Ref, Key, Visible, Accepted); Check (not Accepted);
      P.Encode (Visible, Actual, Result); Check (Result = P.Invalid_Binding);
      O.Bind (Types, Settings, Key, Visible, Accepted); Check (Accepted);
      P.Encode (Visible, Actual, Result); Check (Result = P.Success and Actual = Expected);
      P.Decode (Actual.Data (1 .. CBOR.SE_Offset (Actual.Length)), Restored, Result);
      Check (Result = P.Success and O.Same_Schema (Restored, Visible));
      O.Bind (Types, T.Integer_Type, Key, Visible, Accepted); Check (Accepted);
      P.Encode (Visible, Actual, Result);
      Check (Result = P.Success and Actual.Length = Golden'Length and
             Actual.Data (1 .. Golden'Length) = Golden);
      -- An unused ordinary declaration is also noncanonical on disk.
      -- ["X", product, []] appended to a primitive-root schema.
      Reject (Golden (1 .. 37) & [16#81#, 16#83#, 16#41#, Character'Pos ('X'), 1, 16#80#]);
   end;
   --  Largest metadata catalog/names/part counts: no fixed integer schema.
   for Index in 1 .. T.Maximum_Declarations loop
      declare
         Definition : T.Description;
         Ref : T.Type_Reference;
         Defined : T.Definition_Result;
         Name : String (1 .. 32) := [others => 'T'];
      begin
         Name (31) := Character'Val (Character'Pos ('A') + (Index - 1) / 26);
         Name (32) := Character'Val (Character'Pos ('A') + (Index - 1) mod 26);
         Definition.Identifier := T.Named (Name);
         Definition.Form := T.Sum;
         Definition.Count := T.Maximum_Components;
         for Part in Definition.Parts'Range loop
            Name := [others => 'P'];
            Name (32) := Character'Val (Character'Pos ('A') + Part - 1);
            Definition.Parts (Part) := (T.Named (Name), T.Unit_Type);
         end loop;
         -- Keep all declarations reachable from the root without an
         -- exponentially growing value layout.
         Definition.Parts (1).Payload := T.Last (Registry);
         T.Define (Registry, Definition, Ref, Defined);
         Check (Defined = T.Defined);
      end;
   end loop;
   O.Bind (Registry, T.Last (Registry), Key, Contract, Accepted);
   Check (Accepted);
   Roundtrip;
   Check (Data.Length > 19_000 and Data.Length <= P.Maximum_Encoded_Bytes);
   for Last in 0 .. Data.Length - 1 loop Reject (Data.Data (1 .. CBOR.SE_Offset (Last))); end loop;
   if Ada.Command_Line.Argument_Count = 2 then
      declare
         Value : O.Image := O.Empty (Contract);
         Restored_Value : O.Image;
         Built : O.Build_Result;
         Value_Data : O.Persistence.Packet;
         Value_Result : O.Persistence.Outcome;
         Base : constant String := Ada.Command_Line.Argument (2);
      begin
         O.Append (Value, O.Variant_Cell (2), Built); Check (Built = O.Added);
         O.Append (Value, O.Unit_Cell, Built); Check (Built = O.Added);
         Check (O.Validate (Value, Contract));
         if Ada.Command_Line.Argument (1) = "--emit" then
            Write_Bytes (Base & ".schema", Data.Data (1 .. CBOR.SE_Offset (Data.Length)));
            O.Persistence.Encode (Value, Contract, Value_Data, Value_Result);
            Check (Value_Result = O.Persistence.Success);
            Write_Bytes (Base & ".value", Value_Data.Data (1 .. CBOR.SE_Offset (Value_Data.Length)));
         elsif Ada.Command_Line.Argument (1) = "--check" then
            P.Decode (Read_Bytes (Base & ".schema"), Restored, Result);
            Check (Result = P.Success and Restored = Contract);
            O.Persistence.Decode (Read_Bytes (Base & ".value"), Restored, Restored_Value, Value_Result);
            Check (Value_Result = O.Persistence.Success and Restored_Value = Value);
         else raise Program_Error with "unknown fixture option";
         end if;
      end;
   end if;
   Ada.Text_IO.Put_Line ("CCL portable schema codec: PASS" & Count'Image & " checks");
end Schema_Codec_Tests;
