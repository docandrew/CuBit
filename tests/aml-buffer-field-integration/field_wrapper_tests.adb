with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Field_Wrapper_Tests is
   procedure Clock (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (24, AML_Delays.Unavailable_Provider, Clock);
   use N; use N.Owned;
   use type Integer_Value;
   use type Byte;
   use type AML_Objects.Allocation_Status;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Width : Integer_Width := Bits_64;
   Status : Execution_Status := Returned;
   Checks : Natural := 0;
   Running : Boolean := False;
   Source, Text : Datum;
   Backing : AML_References.Object_Handle;
   Last_Field : Reference;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & Status'Image; end if;
   end Check;
   function Path (S : String) return AML_Names.Name_Result is
      B : Bytes (1 .. S'Length);
   begin
      for I in B'Range loop B (I) := Byte (Character'Pos (S (S'First + (I - 1)))); end loop;
      return AML_Names.Read_Name (B);
   end Path;
   procedure Clock (Value : out Integer_Value; Available : out Boolean) is
      Before : State;
      V, Copy, Bad : Datum;
      Ref : Reference;
      Found : Lookup_Result;
      OK : Boolean;
      ID : AML_Objects.Object_ID;
      Alloc : AML_Objects.Allocation_Status;
      procedure Define (S : String; Item : Datum; Offset, Bits : Integer_Value;
         Buffer_Result : Boolean := False) is
      begin
         Define_Buffer_Field_Value (A, 3, Path (S), Width, Item, Offset, Bits, Buffer_Result, Status);
      end Define;
      procedure Find (S : String) is
      begin
         Found := Resolve (Snapshot (A), 3, Path (S)); Check (Found.Status = N.Found);
         Make_Named_Reference (A, Natural (Found.Node), Ref, OK); Check (OK);
         Check (Kind (A, Found.Node) = Buffer_Field_Object);
      end Find;
   begin
      Value := 0; Available := True;
      if not Running then return; end if;
      Define ("INTF", (Integer_Datum, 16#4847_4645_4443_4241#, Ordinary_Integer), 0, 8);
      Check (Status = Returned); Find ("INTF");
      Read_Buffer_Field (A, Ref, Width, V, Status); Check (Status = Returned and then V.Number = 65);
      Define ("STRF", Text, 8, 8); Check (Status = Returned); Find ("STRF");
      Read_Buffer_Field (A, Ref, Width, V, Status); Check (Status = Returned and then V.Number = 66);
      Define ("BUFF", Source, 0, 8); Check (Status = Returned); Find ("BUFF");
      Last_Field := Ref;
      Before := Snapshot (A);
      Define ("BUFF", (Integer_Datum, 7, Ordinary_Integer), 0, 8);
      Check (Status = Duplicate_Name and then Snapshot (A) = Before);
      Define ("OVER", Text, Integer_Value'Last, 1);
      Check (Status = Buffer_Field_Limit and then Snapshot (A) = Before);
      Define ("ZERO", Text, 0, 0);
      Check (Status = Invalid_Field_Length and then Snapshot (A) = Before);
      Bad := Source; Bad.Object.ID := Text.Object.ID;
      Define ("BAD0", Bad, 0, 8);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      Define ("BAD1", (Reference_Datum, Ref), 0, 8);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      for Mode in Reference_Store_Mode loop
         Store_Reference_Value (A, Ref, Width, (Integer_Datum, 90, Ordinary_Integer), Status, Mode);
         Check (Status = Returned and then Kind (A, Found.Node) = Buffer_Field_Object);
         Check (AML_Objects.Stored_Byte (Value_Store (Snapshot (A)), Source.Object.ID, 0) = 90);
      end loop;
      Copy_And_Attach (A, (Referenced_Destination, Ref), Width, Text, Copy, Status);
      Check (Status = Returned and then Kind (A, Found.Node) = Buffer_Field_Object);
      Check (AML_Objects.Stored_Byte (Value_Store (Snapshot (A)), Source.Object.ID, 0) = 65);
      Copy_And_Attach (A, (Named_Destination, 3, Path ("BUFF")), Width,
        (Integer_Datum, 42, Ordinary_Integer), Copy, Status);
      Check (Status = Returned and then Kind (A, Found.Node) = Buffer_Field_Object);
      Check (AML_Objects.Stored_Byte (Value_Store (Snapshot (A)), Source.Object.ID, 0) = 42);
      Before := Snapshot (A);
      Replace_Value (A, 3, Path ("BUFF"), Width, Text, Status);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      -- Source name replacement cannot change the captured backing identity.
      Replace_Value (A, 0, Path ("SRC0"), Width, (Integer_Datum, 0, Ordinary_Integer), Status);
      Check (Status = Returned);
      Read_Buffer_Field (A, Ref, Width, V, Status); Check (Status = Returned and then V.Number = 42);
      while Values_Used (A).Objects < AML_Objects.Max_Objects loop
         Append (A, Bytes'(1 .. 0 => 0), ID, Alloc); Check (Alloc = AML_Objects.Allocated);
      end loop;
      Before := Snapshot (A);
      Define ("FULL", (Integer_Datum, 1, Ordinary_Integer), 0, 8);
      Check (Status = Value_Limit and then Snapshot (A) = Before);
      Copy_And_Attach (A, (Referenced_Destination, Ref), Width, Text, Copy, Status);
      Check (Status = Value_Limit and then Snapshot (A) = Before);
      Check (Copy.Value_Kind = Integer_Datum and then Copy.Number = 0);
      -- No value allocation for an existing Buffer declaration at full pool.
      Define ("EXST", Source, 0, 1, True); Check (Status = Returned); Find ("EXST");
      Before := Snapshot (A);
      Read_Buffer_Field (A, Ref, Width, V, Status);
      Check (Status = Value_Limit and then Snapshot (A) = Before);
      Store_Reference_Value (A, Ref, Width, (Integer_Datum, 1, Ordinary_Integer), Status);
      Check (Status = Returned);
   end Clock;
   Loaded_Status : Load_Status;
   Result : Execution_Result;
   OK : Boolean;
   V : Datum;
begin
   for W in Integer_Width loop
      Width := W; Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#08#,83,82,67,48,16#11#,5,16#0A#,2,16#81#,16#42#,
        16#08#,83,84,82,48,16#0D#,65,66,0,
        16#14#,10,84,69,83,84,0,16#5B#,16#33#,16#A4#,0), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Make_Source (A, Data_Object (Snapshot (A), 1), Backing, OK); Check (OK);
      Read_Source (A, Backing, Source, Status); Check (Status = Returned);
      Make_Source (A, Data_Object (Snapshot (A), 2), Backing, OK); Check (OK);
      Read_Source (A, Backing, Text, Status); Check (Status = Returned);
      Running := True; Invoke (A, Input, 3, [others => <>], 0, 100, Result); Running := False;
      Check (Result.Status = Returned);
      Read_Buffer_Field (A, Last_Field, Width, V, Status); Check (Status = Unsupported_Value);
   end loop;
   Ada.Text_IO.Put_Line ("field wrapper checks" & Checks'Image);
end Field_Wrapper_Tests;
