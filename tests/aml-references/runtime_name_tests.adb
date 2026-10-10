with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_Objects;
with AML_References;
procedure Runtime_Name_Tests is
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Max_Pending_Members => 2, Max_Pending_Segments => 2);
   use NS; use NS.Owned;
   use type AML_Objects.Allocation_Status;
   use type AML_Objects.State;
   use type AML_References.Node_Incarnation;
   use type Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   OK : Boolean;
   L : Load_Status;
   R : Execution_Result;
   Prior : NS.State;
   ID : AML_Objects.Object_ID;
   Alloc : AML_Objects.Allocation_Status;
   Checks : Natural := 0;
   Name_Op : constant Byte := 16#08#;
   Return_Op : constant Byte := 16#A4#;
   Temp : constant Bytes := [84,69,77,80];
   Method_Name : constant Bytes := [84,69,83,84];
   procedure Check (B : Boolean) is
   begin Checks := Checks + 1; if not B then raise Program_Error with Checks'Image & " " & R.Status'Image; end if; end Check;
   procedure Prepare (Code : Bytes; W : Integer_Width) is
      Method_Data : constant Bytes := Bytes'(16#14#, Byte (Code'Length + 6)) & Method_Name & Bytes'(1 => 0) & Code;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Method_Data, W, L); Check (L = Loaded);
   end Prepare;
   procedure Run (Code : Bytes; W : Integer_Width; Expected : Execution_Status;
                  Value : Integer_Value := 0; Atomic : Boolean := False; Budget : Natural := 100;
                  Reservation_Failure : Boolean := False) is
   begin
      Prepare (Code, W); Prior := Snapshot (A);
      Invoke (A, Input, 1, [others => (Integer_Datum, 3, AML_Decode.Ordinary_Integer)], 0, Budget, R);
      Check (R.Status = Expected);
      if Expected = Returned then Check (R.Value = Value); end if;
      Check (Node_Count (A) = 1);
      if Atomic then Check (Snapshot (A) = Prior); end if;
      if Reservation_Failure then
         Check (Value_Store (Snapshot (A)) = Value_Store (Prior));
         pragma Assert (Allocating_Cleanup_Frame (Snapshot (A), Prior));
         Check (Last_Incarnation (Snapshot (A)) > Last_Incarnation (Prior));
      end if;
      Check (R.Charged <= Budget);
   end Run;
   function Decl (Data : Bytes) return Bytes is (Bytes'(1 => Name_Op) & Temp & Data);
   function Ret (Data : Bytes) return Bytes is (Bytes'(1 => Return_Op) & Data);
begin
   for W in Integer_Width loop
      Run (Decl ([16#0A#,35]) & Ret (Temp), W, Returned, 35);
      Run (Decl ([0]) & Ret (Temp), W, Returned, 0);
      Run (Decl ([1]) & Ret (Temp), W, Returned, 1);
      Run (Decl ([16#0B#,16#34#,16#12#]) & Ret (Temp), W, Returned, 16#1234#);
      Run (Decl ([16#0C#,16#78#,16#56#,16#34#,16#12#]) & Ret (Temp), W, Returned, 16#12345678#);
      Run (Decl ([16#0E#,1,0,0,0,2,0,0,0]) & Ret (Temp), W, Returned,
        (if W = Bits_32 then 1 else 16#0000000200000001#));
      Run (Decl ([16#FF#]) & Ret (Temp), W, Returned,
        (if W = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      Run (Decl ([16#0D#,97,98,99,0]) & Ret (Bytes'(1 => 16#87#) & Temp), W, Returned, 3);
      Run (Decl ([16#11#,5,16#0A#,2,4,5]) & Ret (Bytes'(1 => 16#87#) & Temp), W, Returned, 2);
      Run (Decl ([16#12#,4,1,16#0A#,9]) & Ret (Bytes'(1 => 16#87#) & Temp), W, Returned, 1);
      Run (Decl ([16#13#,5,16#0A#,1,16#0A#,9]) & Ret (Bytes'(1 => 16#87#) & Temp), W, Returned, 1);
      Run (Bytes'(Name_Op,16#5C#) & Temp & Bytes'(16#0A#,35) & Ret (Bytes'(1 => 16#5C#) & Temp), W, Returned, 35);
      Run (Bytes'(Name_Op,16#5E#) & Temp & Bytes'(16#0A#,35) & Ret (Bytes'(1 => 16#5E#) & Temp), W, Returned, 35);
      -- Self binding, nested self binding, and exact candidate rollback.
      Run (Decl (Bytes'(16#12#,6,1) & Temp) & Ret (Bytes'(1 => 16#87#) & Temp), W, Returned, 1);
      Run (Decl (Bytes'(16#12#,6,1) & Temp) & Bytes'(16#70#,16#83#,16#88#) & Temp & Bytes'(0,0,16#60#) & Ret ([16#87#,16#60#]), W, Returned, 1);
      Run (Decl (Bytes'(16#12#,6,1) & Temp) & Bytes'(16#70#,16#0A#,9,16#88#) & Temp & Bytes'(0,0)
        & Ret (Bytes'(16#83#,16#88#) & Temp & Bytes'(0,0)), W, Returned, 9);
      Run (Decl (Bytes'(16#12#,9,1,16#12#,6,1) & Temp)
        & Bytes'(16#70#,16#83#,16#88#) & Temp & Bytes'(0,0,16#60#) & Ret ([16#87#,16#60#]), W, Returned, 1);
      Run (Bytes'(Name_Op,16#5C#) & Temp & Bytes'(16#12#,7,1,16#5C#) & Temp
        & Ret (Bytes'(16#87#,16#5C#) & Temp), W, Returned, 1);
      Run (Bytes'(Name_Op,16#5E#) & Temp & Bytes'(16#12#,7,1,16#5E#) & Temp
        & Ret (Bytes'(16#87#,16#5E#) & Temp), W, Returned, 1);
      Run (Decl (Bytes'(16#12#,10,2) & Temp & Bytes'(78,79,78,69)) & Ret ([0]), W, Unsupported_Value, Atomic => True);
      Run (Decl (Bytes'(16#12#,6,1) & Method_Name) & Ret ([0]), W, Unsupported_Value, Atomic => True);
      Run (Decl ([1]) & Decl ([0]) & Ret (Temp), W, Duplicate_Name);
      Run ([Name_Op], W, Truncated, Atomic => True);
      Run ([Name_Op,84], W, Truncated, Atomic => True);
      Run ([Name_Op,0,0], W, Bad_Name, Atomic => True);
      Run (Bytes'(Name_Op,16#2E#,78,79,78,69) & Temp & Bytes'(1 => 0), W, Unknown_Name, Atomic => True);
      Run (Decl ([]), W, Truncated, Atomic => True);
      Run (Decl ([16#0A#]), W, Truncated, Atomic => True);
      Run (Decl ([16#0D#,97]), W, Truncated, Atomic => True);
      Run (Decl ([16#72#,1,1,0]), W, Unsupported_Value, Atomic => True);
      Run (Decl ([16#71#]) & Temp, W, Unsupported_Value, Atomic => True);
      Run (Decl ([16#68#]), W, Unsupported_Value, Atomic => True);
      Run (Decl ([16#11#,4,16#68#,1,2]), W, Missing_Argument, Reservation_Failure => True);
      Run (Decl ([16#13#,3,16#68#,1]), W, Unsupported_Value, Atomic => True);
      -- A same-named ancestor must not capture the new package's self member.
      Prepare (Decl (Bytes'(16#12#,6,1) & Temp)
        & Bytes'(16#70#,16#83#,16#88#) & Temp & Bytes'(0,0,16#60#) & Ret ([16#8E#,16#60#]), W);
      Load (A, Decl ([16#0A#,123]), W, L); Check (L = Loaded);
      Prior := Snapshot (A);
      Invoke (A, Input, 1, [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)], 0, 100, R);
      Check (R.Status = Returned and then R.Value = 4);
      Check (Node_Count (A) = 2 and then Integer_Data (Snapshot (A), 2) = 123);
      pragma Assert (Allocating_Cleanup_Frame (Snapshot (A), Prior));
      -- Private journal quota failures publish no partial namespace/object state.
      Run (Decl (Bytes'(16#12#,14,3) & Temp & Temp & Temp) & Ret ([0]), W, Value_Limit, Atomic => True);
      Run (Decl (Bytes'(16#12#,20,2,16#2E#) & Method_Name & Temp
        & Bytes'(1 => 16#2E#) & Method_Name & Temp) & Ret ([0]), W, Value_Limit, Atomic => True);
      for Fuel in 0 .. 3 loop
         Run (Decl ([16#0A#,35]) & Ret (Temp), W,
           (if Fuel = 3 then Returned else Budget_Exceeded),35,Atomic => Fuel=0,Budget => Fuel);
      end loop;
      Prepare (Decl ([16#0D#,97,0]) & Ret ([0]), W);
      Append (A,[1 .. AML_Objects.Max_Bytes => 0],ID,Alloc); Check (Alloc = AML_Objects.Allocated);
      Prior := Snapshot (A);
      Invoke (A,Input,1,[others=>(Integer_Datum,0, AML_Decode.Ordinary_Integer)],0,100,R);
      Check (R.Status = Value_Limit and then Snapshot (A) = Prior);
   end loop;
   Ada.Text_IO.Put_Line ("RUNTIME-NAME PASS" & Checks'Image);
end Runtime_Name_Tests;
