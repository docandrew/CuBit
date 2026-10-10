with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Owned_Frame_Root_Tests is
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Capture, Max_Frame_Roots => 2);
   use N; use N.Owned;
   use type AML_Objects.Allocation_Status;
   use type Integer_Value;
   use type Object_Root_Set;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Scratch : Root_Workspace;
   Keep, Expected : Object_Root_Set := [others => False];
   Root_Status : Root_Trace_Status;
   ID, Counter : AML_Objects.Object_ID;
   Source : AML_References.Object_Handle;
   Argument : Datum;
   Calls, Checks : Natural := 0;
   type Scenario is (Normal, Full_Pool, Drop_Argument, Reentry, No_Arguments);
   Current : Scenario;
   Width : Integer_Width;
   function Segment (Name : String) return Bytes is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Data;
   end Segment;
   function Method (Name : String; Args : Byte; Code : Bytes) return Bytes is
     (Bytes'(16#14#,Byte (Code'Length + 6)) & Segment (Name) & Bytes'(1 => Args) & Code);
   Timer : constant Bytes := [16#5B#,16#33#];
   Fixture : constant Bytes :=
     Method ("OUTR", 1, Timer & Segment ("INNR") & Bytes'(1 => 16#68#) & Timer & Bytes'(16#A4#,16#68#)) &
     Method ("INNR", 1, Bytes'(1 => 16#75#) & Segment ("CNT0") & Timer & Bytes'(16#A4#,16#68#)) &
     Method ("MIDL", 1, Timer & Segment ("INNR") & Bytes'(16#68#,16#A4#,0)) &
     Method ("LIMT", 1, Timer & Segment ("MIDL") & Bytes'(16#68#,16#A4#,0)) &
     Method ("DROP", 1, Timer & Bytes'(16#70#,1,16#68#) & Timer & Bytes'(16#A4#,0)) &
     Method ("NARG", 0, Timer & Bytes'(16#A4#,0)) &
     Bytes'(1 => 16#08#) & Segment ("CNT0") & Bytes'(1 => 0);
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Current'Image & Checks'Image & " timer" & Calls'Image; end if;
   end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      Before : constant N.State := Snapshot (A);
      Count_Before : constant Natural := Frame_Root_Count (A);
      OK : Boolean;
      Nested : Execution_Result;
      Root_Pin : Retained_Root;
      Retention : Invocation_Retention_Status;
      Retained_Before : constant Retained_Root_Count := Retained_Count (A);
   begin
      Value := 0; Available := True; Calls := Calls + 1;
      Check (Count_Before = (if Current = Drop_Argument then 1 elsif Calls = 2 then 2 else 1));
      Reset (A, OK);
      Check (not OK and then Snapshot (A) = Before and then Frame_Root_Count (A) = Count_Before);
      Expected := [others => False]; Expected (Counter) := True;
      Expected (ID) := Current /= No_Arguments and then not (Current = Drop_Argument and then Calls = 2);
      Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
      Check (Root_Status = Roots_Traced and then Keep = Expected);
      if Current = Reentry and then Calls = 1 then
         Invoke (A, Input, 6, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100, Nested);
         Check (Nested.Status = Unsupported_Value and then Nested.Charged = 0 and then Snapshot (A) = Before);
         Invoke_Retained (A, Input, 6, [others => (Integer_Datum,0,Ordinary_Integer)], 0, 100,
           Nested, Root_Pin, Retention);
         Check (Retention = Invocation_Busy and then Nested.Status = Unsupported_Value
           and then Nested.Charged = 0 and then Root_Pin = No_Retained_Root
           and then Retained_Count (A) = Retained_Before and then Snapshot (A) = Before);
         Check (Frame_Root_Count (A) = Count_Before);
         Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
         Check (Root_Status = Roots_Traced and then Keep = Expected);
      end if;
   end Capture;
begin
   for W in Integer_Width loop
      Width := W;
      for Mode in Scenario loop
         Current := Mode; Calls := 0;
         declare
            OK : Boolean;
            Loaded_Status : Load_Status;
            Allocated : AML_Objects.Allocation_Status;
            Status : Execution_Status;
            Result : Execution_Result;
            Root_Pin : Retained_Root;
            Retention : Invocation_Retention_Status;
            Released_Status : Release_Status;
            Method_ID : constant Node_ID := (case Current is
              when Normal | Reentry => 1, when Full_Pool => 4, when Drop_Argument => 5, when No_Arguments => 6);
         begin
            Reset (A, OK); Check (OK);
            Load (A, Fixture, Width, Loaded_Status); Check (Loaded_Status = Loaded);
            Counter := Data_Object (Snapshot (A), 7);
            Append (A, [16#55#], ID, Allocated); Check (Allocated = AML_Objects.Allocated);
            Make_Source (A, ID, Source, OK); Check (OK);
            Read_Source (A, Source, Argument, Status); Check (Status = Returned);
            if Current = Reentry then
               Invoke_Retained (A, Input, Method_ID,
                 [0 => Argument, others => (Integer_Datum,0,Ordinary_Integer)], 1, 100,
                 Result, Root_Pin, Retention);
               Check (Retention = Result_Retained);
               Release (A, Root_Pin, Released_Status); Check (Released_Status = Released);
            else
               Invoke (A, Input, Method_ID,
                 [0 => Argument, others => (Integer_Datum,0,Ordinary_Integer)],
                 (if Current = No_Arguments then 0 else 1), 100, Result);
            end if;
            Check (Result.Status = (case Current is when Normal | Reentry => Object_Returned,
              when Full_Pool => Value_Limit, when Drop_Argument | No_Arguments => Returned));
            Check (Calls = (case Current is when Normal | Reentry => 3, when No_Arguments => 1, when others => 2));
            Check (Frame_Root_Count (A) = 0);
            Check (Integer_Data (Snapshot (A), 7) = (if Current in Normal | Reentry then 1 else 0));
            Expected := [others => False]; Expected (Counter) := True;
            Trace_Owner_Roots (A, [], Scratch, Keep, Root_Status);
            Check (Root_Status = Roots_Traced and then Keep = Expected);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("OWNED FRAME ROOTS" & Checks'Image);
   Ada.Text_IO.Put_Line ("Hosted arena bits" & A'Size'Image);
end Owned_Frame_Root_Tests;
