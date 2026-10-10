with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Frame_Handles;
with AML_References;
with AML_Table_Backing;
procedure Increment_Tests is
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type AML_Frame_Handles.Invocation_Serial;
   use type Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   R : Execution_Result;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks : Natural := 0;
   NVAR : constant Bytes := [78,86,65,82];
   MARK : constant Bytes := [77,65,82,75];
   PKG0 : constant Bytes := [80,75,71,48];
   BUF0 : constant Bytes := [66,85,70,48];
   STR0 : constant Bytes := [83,84,82,48];
   NEXT : constant Bytes := [78,69,88,84];
   function Method_Data (Name, Code : Bytes; Flags : Byte := 0) return Bytes is
     (Bytes'(16#14#,Byte(6+Code'Length)) & Name & Bytes'(1=>Flags) & Code);
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image & R.Status'Image; end if;
   end Check;
   procedure Run (Code : Bytes; Width : Integer_Width; Expected : Execution_Status;
                  Value : Integer_Value := 0; Arg : Integer_Value := 0;
                  Mark_Value : Integer_Value := 0; Budget : Natural := 100;
                  Helper_Flags : Byte := 0;
                  Helper : Bytes := [16#A4#,0]) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(1=>16#08#) & NVAR & Bytes'(1=>1)
        & Bytes'(1=>16#08#) & MARK & Bytes'(1=>0)
        & Bytes'(1=>16#08#) & PKG0 & Bytes'(16#12#,4,1,16#0A#,7)
        & Bytes'(1=>16#08#) & BUF0 & Bytes'(16#11#,3,1,7)
        & Bytes'(1=>16#08#) & STR0 & Bytes'(16#0D#,48,70,0)
        & Method_Data (NEXT, Helper, Helper_Flags)
        & Method_Data ([84,69,83,84], Code, 1), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 7, [others => (Integer_Datum,Arg,Ordinary_Integer)], 1, Budget, R);
      Check (R.Status = Expected);
      if Expected = Returned then
         if R.Value /= Value then
            Ada.Text_IO.Put_Line ("MISMATCH " & Width'Image & " actual" & R.Value'Image & " expected" & Value'Image);
            for B of Code loop Ada.Text_IO.Put (B'Image & " "); end loop;
            Ada.Text_IO.New_Line;
         end if;
         Check (R.Value = Value);
      end if;
      Check (Integer_Data (Snapshot (A), 2) = Mark_Value);
      Check (R.Charged <= Budget);
   end Run;
   function Normal (Value : Integer_Value; Width : Integer_Width) return Integer_Value is
     (if Width = Bits_32 then Value mod 2**32 else Value);
   type Numbers is array (Positive range <>) of Integer_Value;
   Values : constant Numbers := [0,1,16#FFFF_FFFF#,16#1_0000_0000#,Integer_Value'Last];
begin
   for Width in Integer_Width loop
      for Value of Values loop
         Run ([16#A4#,Increment_Op,16#68#], Width, Returned, Normal(Value+1,Width), Value);
         Run ([16#A4#,Decrement_Op,16#68#], Width, Returned, Normal(Normal(Value,Width)-1,Width), Value);
         Run ([16#70#,16#68#,16#60#,Increment_Op,16#60#,16#A4#,16#60#],
              Width,Returned,Normal(Value+1,Width),Value);
         Run ([16#70#,16#68#,16#60#,Decrement_Op,16#60#,16#A4#,16#60#],
              Width,Returned,Normal(Normal(Value,Width)-1,Width),Value);
      end loop;
      Run (Bytes'(16#A4#,Increment_Op)&NVAR,Width,Returned,2);
      Run (Bytes'(16#A4#,Decrement_Op)&NVAR,Width,Returned,0);
      Run (Bytes'(1=>Increment_Op)&NVAR&Bytes'(1=>16#A4#)&NVAR,Width,Returned,2);
      Run ([16#A4#,Increment_Op,16#60#],Width,Uninitialized);
      Run ([16#A4#,Increment_Op],Width,Truncated);
      Run ([16#A4#,Increment_Op,0],Width,Unsupported_Value);
      Run ([16#A4#,Increment_Op,Extended_Op,Debug_Extension],Width,Unsupported_Value);
      Run (Bytes'(16#A4#,Increment_Op,16#88#)&PKG0&Bytes'(0,0),Width,Returned,8);
      Run (Bytes'(Increment_Op,16#88#)&PKG0&Bytes'(0,0,16#A4#,16#83#,16#88#)&PKG0&Bytes'(0,0),Width,Returned,8);
      Run (Bytes'(16#A4#,Decrement_Op,16#88#)&BUF0&Bytes'(0,0),Width,Returned,6);
      Run (Bytes'(16#A4#,Increment_Op,16#88#)&PKG0&NEXT&Bytes'(1=>0),Width,Returned,8,
           Mark_Value=>1,Helper=>Bytes'(1=>Increment_Op)&MARK&Bytes'(16#A4#,0));
      Run (Bytes'(16#A4#,Increment_Op,16#88#)&PKG0&NEXT&Bytes'(1=>0),Width,Unsupported_Value,
           Mark_Value=>1,Helper=>Bytes'(1=>Increment_Op)&MARK&Bytes'(16#A4#,16#0A#,9));
      Run (Bytes'(16#A4#,Increment_Op,16#88#)&PKG0&Bytes'(0,0),Width,Budget_Exceeded,Budget=>2);
      for Budget in 0 .. 4 loop
         Run ([16#A4#,Increment_Op,16#68#],Width,
              (if Budget<3 then Budget_Exceeded else Returned),Value=>1,Budget=>Budget);
      end loop;
      Run (Bytes'(1=>16#A4#)&NEXT&Bytes'(1=>16#71#)&NVAR,Width,Unsupported_Value,
           Helper_Flags=>1,Helper=>[16#A4#,Increment_Op,16#68#]);
      Run (Bytes'(16#70#,16#0A#,5,16#60#)&NEXT&Bytes'(16#71#,16#60#,16#A4#,16#60#),Width,Unsupported_Value,
           Helper_Flags=>1,Helper=>[16#A4#,Increment_Op,16#68#]);
      Run (Bytes'(16#70#,16#71#)&NVAR&Bytes'(16#60#,Increment_Op,16#60#,16#A4#)&NVAR,Width,Unsupported_Value);
      Run ([16#70#,1,16#60#,16#A4#,Increment_Op,16#60#],Width,Returned,2);
      Check (R.Origin = Ordinary_Integer);
      Run ([16#70#,16#0D#,48,70,0,16#60#,16#A4#,Increment_Op,16#60#],Width,Returned,16);
      Run (Bytes'(16#A4#,Increment_Op)&STR0,Width,Returned,16);
      Run (Bytes'(1=>Increment_Op)&STR0&Bytes'(16#A4#,16#87#)&STR0,Width,Returned,(if Width=Bits_32 then 8 else 16));
      Run (Bytes'(1=>Increment_Op)&BUF0&Bytes'(16#A4#,16#83#,16#88#)&BUF0&Bytes'(0,0),Width,Returned,8);
      Run (Bytes'(1=>16#A4#)&NEXT&Bytes'(1=>16#71#)&STR0,Width,Unsupported_Value,
           Helper_Flags=>1,Helper=>[16#A4#,Increment_Op,16#68#]);
      Run (Bytes'(16#72#,1,1)&STR0&Bytes'(16#A4#,16#87#)&STR0,
           Width,Returned,(if Width=Bits_32 then 8 else 16));
      Run (Bytes'(16#72#,1,1)&BUF0&Bytes'(16#A4#,16#83#,16#88#)&BUF0&Bytes'(0,0),
           Width,Returned,2);
      Run (Bytes'(16#A4#,16#70#,16#11#,2,0)&NVAR,Width,Empty_Buffer);
      Run (Bytes'(16#70#,16#11#,2,0)&STR0&Bytes'(16#A4#,16#87#)&STR0,Width,Returned,0);
      -- Reference-valued Local/Arg operands fail integer conversion; they are
      -- not recursively followed, matching the pinned ACPICA observations.
      -- Named String/Buffer updates now use destination-preserving owner conversion.
      declare
         Foreign : Arena;
         Ref : AML_References.Reference;
         Before : NS.State with Ghost;
         Prior_Invocation : AML_Frame_Handles.Invocation_Serial;
      begin
         Reset (Foreign, OK); Check (OK);
         Load (Foreign,Bytes'(1=>16#08#)&NVAR&Bytes'(1=>1),Width,Loaded_Status);
         Check (Loaded_Status = Loaded);
         Make_Named_Reference (Foreign,1,Ref,OK); Check (OK);
         Run ([16#A4#,Increment_Op,16#68#],Width,Returned,1);
         Before := Snapshot (A); Prior_Invocation := Invocation_Count (A);
         Invoke (A,Input,7,[others=>(Reference_Datum,Ref)],1,100,R);
         Check (R.Status = Unsupported_Value);
         Check (Invocation_Count (A) = Prior_Invocation + 1);
         pragma Assert (Snapshot (A) = Before);
         Make_Named_Reference (A,1,Ref,OK); Check (OK);
         Run ([16#A4#,Decrement_Op,16#68#],Width,Returned,Normal(Integer_Value'Last,Width));
         Before := Snapshot (A); Prior_Invocation := Invocation_Count (A);
         Invoke (A,Input,7,[others=>(Reference_Datum,Ref)],1,100,R);
         Check (R.Status = Unsupported_Value);
         Check (Invocation_Count (A) = Prior_Invocation + 1);
         pragma Assert (Snapshot (A) = Before);
      end;
      -- Same arithmetic/source expression as preserved ONCE; helper now uses Increment.
      Run (Bytes'(16#A4#,16#72#,16#77#,16#70#)&NEXT&Bytes'(Extended_Op,Debug_Extension,16#0A#,10,0)&MARK&Bytes'(1=>0),
           Width,Returned,71,Mark_Value=>1,Helper=>Bytes'(1=>Increment_Op)&MARK&Bytes'(16#A4#,16#0A#,7));
   end loop;
   Ada.Text_IO.Put_Line ("INCREMENT/DECREMENT: PASS" & Checks'Image);
end Increment_Tests;
