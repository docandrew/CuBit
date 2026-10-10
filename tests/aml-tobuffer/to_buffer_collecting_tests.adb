with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure To_Buffer_Collecting_Tests is
   package N is new AML_Namespace (32, Max_Retained_Roots => 8,
     Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Collection_Count;
   use type AML_Execute.Execution_Status;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Args : constant C.Arguments := [others => (C.Immediate_Argument, 0)];
   Outcome : C.Result;
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Checks : Natural := 0;
   function Name (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop; return Data; end Name;
   function Method (Text : String; Code : Bytes) return Bytes is
     (Bytes'(16#14#, Byte (Code'Length + 6)) & Name (Text) & Bytes'(1 => 0) & Code);
   Fixture : constant Bytes :=
     Method ("KEEP", Bytes'(16#A4#,16#96#,16#0D#,65,66,0,0)) &
     Method ("TEMP", Bytes'(16#70#,16#11#,5,16#0A#,2,7,8,16#60#,
       16#96#,16#0D#,65,66,0,16#88#,16#60#,0,0,16#A4#,16#60#)) &
     Method ("NEST", Bytes'(16#A4#,16#96#) & Name ("KEEP") & Bytes'(1 => 0));
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & Outcome.Status'Image; end if; end Check;
   procedure Verify (H : C.Value_Handle; Expected : Bytes) is
      Data : Bytes (1 .. 8); Copied : Natural;
   begin
      C.Read_Bytes (A, H, 0, Data, Copied, Status);
      Check (Status = C.Available and then Copied = Expected'Length);
      Check (Data (1 .. Copied) = Expected);
   end Verify;
begin
   for Width in Integer_Width loop
      C.Reset (A, Status); Check (Status = C.Available);
      C.Load (A, Fixture, Width, Loaded, Status); Check (Status = C.Available and then Loaded = N.Loaded);
      C.Seal (A, Report, Status); Check (Status = C.Available);
      C.Invoke (A, Input, 1, Args, 0, 100, Outcome, Status);
      Check (Status = C.Available and then Outcome.Status = AML_Execute.Object_Returned);
      declare Held : C.Value_Handle := Outcome.Handle; begin
         for I in 1 .. 32 loop
            C.Invoke (A, Input, 2, Args, 0, 100, Outcome, Status);
            Check (Status = C.Available and then Outcome.Status = AML_Execute.Object_Returned);
            declare H : C.Value_Handle := Outcome.Handle; begin
               Verify (H, Bytes'(65,8)); C.Release (A, H, Status); Check (Status = C.Available);
            end;
            Verify (Held, Bytes'(65,66,0));
         end loop;
         C.Invoke (A, Input, 3, Args, 0, 100, Outcome, Status);
         Check (Status = C.Available and then Outcome.Status = AML_Execute.Object_Returned);
         declare H : C.Value_Handle := Outcome.Handle; begin
            Verify (H, Bytes'(65,66,0)); C.Release (A, H, Status); Check (Status = C.Available);
         end;
         Verify (Held, Bytes'(65,66,0)); C.Release (A, Held, Status); Check (Status = C.Available);
      end;
      Check (C.Reclamation_Metrics (A).Completed > 0 and then C.Reclamation_Metrics (A).Freed_Objects > 0
        and then C.Reclamation_Metrics (A).Rejected = 0);
   end loop;
   Ada.Text_IO.Put_Line ("ToBuffer collecting checks" & Checks'Image);
end To_Buffer_Collecting_Tests;
