with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Names;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Field_Package_Compat_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   use type Integer_Value;
   use type Byte;
   use type AML_Objects.Object_Kind;
   use type AML_References.Reference;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   R : Execution_Result;
   Status : Execution_Status;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Ref : Reference;
   Checks : Natural := 0;
   function Name (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Data;
   end Name;
   function Path (Text : String) return AML_Names.Name_Result is
     (AML_Names.Read_Name (Name (Text)));
   function Method (Code : Bytes) return Bytes is
     ([16#14#, Byte (Code'Length + 6)] & Name ("TEST") & Bytes'(1 => 0) & Code);
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & R.Status'Image; end if;
   end Check;
begin
   for Width in Integer_Width loop
      -- Named self-package remains constructible and its NAME member retains
      -- the existing metadata behavior; no field read is involved.
      Reset (A, OK); Check (OK);
      Load (A, Method ([16#08#] & Name ("PKG0") & [16#12#,6,1] & Name ("PKG0") &
        [16#A4#,16#87#] & Name ("PKG0")), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 1, [others => <>], 0, 100, R);
      Check (R.Status = Returned and then R.Value = 1);

      -- Manufacture an ordinary existing Reference_Object through supported
      -- owner APIs, independent of the evaluator's CopyObject implementation.
      Reset (A, OK); Check (OK);
      Load (A, [16#08#] & Name ("SRC0") & [16#0A#,65,16#08#] & Name ("REF0") & [0] &
        Method ([16#A4#,16#12#,6,1] & Name ("REF0")), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Make_Named_Reference (A, 1, Ref, OK); Check (OK);
      Replace_Value (A, 0, Path ("REF0"), Width, (Reference_Datum, Ref), Status);
      Check (Status = Returned and then Kind (A, 2) = Reference_Object);
      Invoke (A, Input, 3, [others => <>], 0, 100, R);
      Check (R.Status = Object_Returned);
      declare
         Store : constant AML_Objects.State := Value_Store (Snapshot (A));
         Leaf : constant AML_Objects.Object_ID := AML_Objects.Element (Store, R.Object.ID, 0);
      begin
         Check (AML_Objects.Kind (Store, Leaf) = AML_Objects.Reference_Object);
         Check (AML_Objects.Reference_Data (Store, Leaf) = Ref);
         Check (Leaf = Data_Object (Snapshot (A), 2));
      end;
      for Named in Boolean loop
         for Wide in Boolean loop
            declare
               Data : constant Bytes := [16#11#,11,16#0A#,8,129,2,0,0,0,0,0,0];
               Field_Code : constant Bytes :=
                 (if Wide then [16#8F#] & Data & [0]
                  else [16#5B#,16#13#] & Data & [1,16#0A#,9]) & Name ("FLD0");
               -- A nested package exercises journal records at both depths.
               Package_Code : constant Bytes := [16#12#,9,1,16#12#,6,1] & Name ("FLD0");
               Code : constant Bytes := Field_Code &
                 (if Named then [16#08#] & Name ("PKG0") & Package_Code & [16#A4#] & Name ("PKG0")
                  else [16#A4#] & Package_Code);
            begin
               Reset (A, OK); Check (OK);
               Load (A, Method (Code), Width, Loaded_Status); Check (Loaded_Status = Loaded);
               Invoke (A, Input, 1, [others => <>], 0, 100, R);
               Check (R.Status = Object_Returned);
               declare
                  Store : constant AML_Objects.State := Value_Store (Snapshot (A));
                  Nested : constant AML_Objects.Object_ID := AML_Objects.Element (Store, R.Object.ID, 0);
                  Leaf : constant AML_Objects.Object_ID := AML_Objects.Element (Store, Nested, 0);
               begin
                  if Wide and Width = Bits_64 then
                     Check (AML_Objects.Kind (Store, Leaf) = AML_Objects.Integer_Object);
                     Check (AML_Objects.Integer_Data (Store, Leaf) = 641);
                  else
                     Check (AML_Objects.Kind (Store, Leaf) = AML_Objects.Buffer_Object);
                     Check (AML_Objects.Length (Store, Leaf) = (if Wide then 8 else 2));
                     Check (AML_Objects.Stored_Byte (Store, Leaf, 0) = (if Wide then 129 else 64));
                     Check (AML_Objects.Stored_Byte (Store, Leaf, 1) = (if Wide then 2 else 1));
                  end if;
               end;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("field package compatibility checks" & Checks'Image);
end Field_Package_Compat_Tests;
