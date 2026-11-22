with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_References;
with AML_Identity;
with AML_Table_Backing;
procedure Name_Member_Tests is
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Capture);
   use NS; use NS.Owned;
   use type AML_References.Reference_Kind;
   use type AML_References.Reference;
   use type Integer_Value;
   A, Foreign : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Saved, Old : AML_References.Reference := AML_References.No_Reference;
   Item : Datum;
   Status : Execution_Status;
   Result : Execution_Result;
   Checks : Natural := 0;
   OK : Boolean;
   Loaded_Status : Load_Status;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image & Status'Image; end if;
   end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Located : Lookup_Result;
      Source : AML_References.Object_Handle;
      Index : Reference;
      Before : constant NS.State := Snapshot (A);
   begin
      Value := 0; Available := True;
      Located := Resolve (Snapshot (A), 1, AML_Names.Read_Name (Bytes'(84,69,77,80)));
      Check (Located.Status = Found);
      Make_Source (A, Data_Object (Snapshot (A), Located.Node), Source, OK); Check (OK);
      Make_Index (A, Source, 0, Index, Status); Check (Status = Returned);
      Resolve_Value (A, Index, Item, Status);
      Check (Status = Returned and then Item.Value_Kind = Reference_Datum);
      Saved := Item.Ref;
      Check (AML_References.Kind (Saved) = AML_References.Name_Member);
      Check (Name_Member_Matches (A, Saved) and then not Matches (A, Saved));
      Resolve_Name_Member (A, Saved, Item, Status);
      Check (Status = Returned and then Item.Value_Kind = Object_Datum and then Item.Object.Type_Code = 4);
      Resolve_Name_Member (Foreign, Saved, Item, Status); Check (Status = Unsupported_Value);
      Store_Reference_Value (A, Saved, Bits_64, (Integer_Datum,9,Ordinary_Integer), Status);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      if Old /= AML_References.No_Reference then
         Resolve_Name_Member (A, Old, Item, Status); Check (Status = Unsupported_Value);
      end if;
   end Capture;
   Code : constant Bytes := [16#14#,24,84,69,83,84,0,
     16#08#,84,69,77,80,16#12#,6,1,84,69,77,80,
     16#70#,16#5B#,16#33#,16#60#,16#A4#,1];
begin
   for Width in Integer_Width loop
      Reset (A, OK); Check (OK); Reset (Foreign, OK); Check (OK);
      Old := AML_References.No_Reference;
      Load (A, Code, Width, Loaded_Status); Check (Loaded_Status = Loaded);
      declare
         Legacy : NS.State := Snapshot (A);
         Before : constant NS.State := Legacy;
      begin
         Invoke_Mutable (Legacy, 1, [others => 0], 0, 100, Result);
         Check (Result.Status = Unsupported_Value and then Legacy = Before);
      end;
      for Repeat in 1 .. 2 loop
         Invoke (A, Input, 1, [others => <>],0,100,Result);
         Check (Result.Status = Returned and then Result.Value = 1);
         Resolve_Name_Member (A, Saved, Item, Status); Check (Status = Unsupported_Value);
         Old := Saved;
      end loop;
      declare Before : constant NS.State := Snapshot (A); begin
         Resolve_Name_Member (A, AML_References.Bind_Name_Member (AML_Identity.No_Identity,1,1), Item, Status);
         Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("NAME MEMBER: PASS" & Checks'Image);
end Name_Member_Tests;
