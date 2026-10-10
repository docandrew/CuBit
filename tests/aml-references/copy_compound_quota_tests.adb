with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_Frame_Handles;
with AML_Table_Backing;
with Test_Namespace;
procedure Copy_Compound_Quota_Tests is
   package NS renames Test_Namespace;
   package Owner renames NS.Owned;
   use type NS.Load_Status;
   use type AML_Objects.Usage;
   use type AML_Objects.State;
   use type AML_Objects.Allocation_Status;
   use type AML_Frame_Handles.Invocation_Serial;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   -- Name(BUF0,Buffer(1){7}); Name(DST0,42).
   Data : constant Bytes :=
     [16#08#,16#42#,16#55#,16#46#,16#30#,16#11#,4,16#0A#,1,7,
      16#08#,16#44#,16#53#,16#54#,16#30#,16#0A#,42];
   -- Method(TEST,0){CopyObject(BUF0,DST0);Return(One)}
   Direct : constant Bytes :=
     [16#14#,17,16#54#,16#45#,16#53#,16#54#,0,
      16#9D#,16#42#,16#55#,16#46#,16#30#,16#44#,16#53#,16#54#,16#30#,
      16#A4#,1];
   -- Method(AUX0,1){CopyObject(BUF0,Arg0);Return(One)}
   -- Method(TEST,0){AUX0(RefOf(DST0));Return(One)}
   Indirect : constant Bytes :=
     [16#14#,14,16#41#,16#55#,16#58#,16#30#,1,
      16#9D#,16#42#,16#55#,16#46#,16#30#,16#68#,16#A4#,1,
      16#14#,17,16#54#,16#45#,16#53#,16#54#,0,
      16#41#,16#55#,16#58#,16#30#,16#71#,16#44#,16#53#,16#54#,16#30#,
      16#A4#,1];
   type Quota_Kind is (Object_Quota, Byte_Quota);
begin
   for W in Integer_Width loop
      for Via_Argument in Boolean loop
         for Quota in Quota_Kind loop
            declare
               A : Owner.Arena;
               Input : aliased AML_Table_Backing.State (1, 1);
               OK : Boolean;
               Loaded : NS.Load_Status;
               ID : AML_Objects.Object_ID;
               Allocated : AML_Objects.Allocation_Status;
               Result : Execution_Result;
               Args : constant Value_Arguments := [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)];
               Method : constant NS.Node_ID := (if Via_Argument then 4 else 3);
            begin
               Owner.Reset (A, OK); Check (OK);
               Owner.Load (A, Data & (if Via_Argument then Indirect else Direct), W, Loaded);
               Check (Loaded = NS.Loaded);
               if Quota = Object_Quota then
                  while Owner.Values_Used (A).Objects < AML_Objects.Max_Objects - 1 loop
                     Owner.Append (A, Bytes'(1 .. 0 => 0), ID, Allocated);
                     Check (Allocated = AML_Objects.Allocated);
                  end loop;
               else
                  declare
                     Padding : constant Bytes
                       (1 .. AML_Objects.Max_Bytes - Owner.Values_Used (A).Bytes - 1) := [others => 0];
                  begin
                     Owner.Append (A, Padding, ID, Allocated);
                     Check (Allocated = AML_Objects.Allocated);
                  end;
               end if;
               declare
                  Before : constant NS.State := Owner.Snapshot (A);
                  Usage : constant AML_Objects.Usage := Owner.Values_Used (A);
                  Serial : constant AML_Frame_Handles.Invocation_Serial := Owner.Invocation_Count (A);
               begin
                  -- Operands are existing Name/RefOf reads: no allocation or mutation.
                  -- Only the CopyObject operation can consume the remaining capacity.
                  Owner.Invoke (A, Input, Method, Args, 0, 100, Result);
                  Check (Result.Status = Value_Limit);
                  Check (Result.Charged <= 100);
                  Check (Owner.Invocation_Count (A) = Serial + 1);
                  Ada.Text_IO.Put_Line ("CASE " & W'Image & " ARG=" & Via_Argument'Image & " " & Quota'Image);
                  Ada.Text_IO.Put_Line ("STATUS " & Result.Status'Image);
                  Ada.Text_IO.Put_Line ("BEFORE objects" & Usage.Objects'Image & " bytes" & Usage.Bytes'Image & " elements" & Usage.Elements'Image);
                  Ada.Text_IO.Put_Line ("AFTER objects" & Owner.Values_Used (A).Objects'Image & " bytes" & Owner.Values_Used (A).Bytes'Image & " elements" & Owner.Values_Used (A).Elements'Image);
                  Check (Owner.Values_Used (A) = Usage);
                  Check (NS.Value_Store (Owner.Snapshot (A)) = NS.Value_Store (Before));
                  Check (NS.Data_Object (Owner.Snapshot (A), 2) = NS.Data_Object (Before, 2));
                  Check (NS.Cleanup_Frame (Owner.Snapshot (A), Before));
               end;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("COPY COMPOUND QUOTA" & Checks'Image);
end Copy_Compound_Quota_Tests;
