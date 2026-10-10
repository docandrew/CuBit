with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Resource_Collecting_Tests is
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
     Method ("KEEP", [16#A4#,16#84#,16#11#,5,16#0A#,2,16#79#,0,16#11#,2,0,0]) &
     Method ("SRC0", [16#A4#,16#11#,5,16#0A#,2,16#79#,0]) &
     Method ("RGT0", [16#A4#,16#11#,2,0]) &
     Method ("TGT0", [16#70#,16#0D#,88,0,16#60#,16#A4#,0]) &
     Method ("NEST", [16#A4#,16#84#] & Name ("SRC0") & Name ("RGT0") & Bytes'(1 => 0)) &
     Method ("TARG", [16#70#,16#12#,3,1,0,16#60#,16#A4#,16#84#] &
       Name ("SRC0") & Name ("RGT0") & [16#88#,16#60#] & Name ("TGT0") & Bytes'(1 => 0)) &
     Method ("MISS", [16#A4#,16#84#,16#11#,2,0,16#11#,2,0]);
   Expected : constant Bytes := [16#79#,0];
   Expected_Length : constant Natural := 2;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & Outcome.Status'Image; end if; end Check;
   procedure Verify (H : C.Value_Handle) is
      Data : Bytes (1 .. 8); Copied : Natural;
   begin
      C.Read_Bytes (A, H, 0, Data, Copied, Status);
      Check (Status = C.Available and then Copied = Expected_Length);
      Check (Data (1 .. Copied) = Expected (1 .. Expected_Length));
   end Verify;
begin
   for Width in Integer_Width loop
      C.Reset (A, Status); Check (Status = C.Available);
      C.Load (A, Fixture, Width, Loaded, Status); Check (Status = C.Available and then Loaded = N.Loaded);
      C.Seal (A, Report, Status); Check (Status = C.Available);
      -- Return + ConcatenateResTemplate + two Buffers; simple Null target is uncharged.
      C.Invoke (A, Input, 1, Args, 0, 4, Outcome, Status);
      Check (Status = C.Available and then Outcome.Status = AML_Execute.Object_Returned and then Outcome.Charged = 4);
      declare Held : C.Value_Handle := Outcome.Handle; begin
         Verify (Held);
         for I in 1 .. 8 loop
            declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
            C.Invoke (A, Input, 5, Args, 0, 100, Outcome, Status);
            Check (Status = C.Available and then Outcome.Status = AML_Execute.Object_Returned);
            declare H : C.Value_Handle := Outcome.Handle; begin
               Verify (H); C.Release (A, H, Status); Check (Status = C.Available);
            end;
            Verify (Held);
            Check (C.Reclamation_Metrics (A).Completed > Before);
            end;
         end loop;
         declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
         C.Invoke (A, Input, 6, Args, 0, 100, Outcome, Status);
         Check (Status = C.Available and then Outcome.Status = AML_Execute.Object_Returned);
         declare H : C.Value_Handle := Outcome.Handle; begin
            Verify (H); C.Release (A, H, Status); Check (Status = C.Available);
         end;
         Check (C.Reclamation_Metrics (A).Completed > Before);
         end;
         C.Invoke (A, Input, 1, Args, 0, 3, Outcome, Status);
         Check (Status = C.Available and then Outcome.Status = AML_Execute.Budget_Exceeded and then Outcome.Charged = 3);
         Verify (Held);
         C.Invoke (A, Input, 7, Args, 0, 100, Outcome, Status);
         Check (Status = C.Available and then Outcome.Status = AML_Execute.Truncated);
         Verify (Held); C.Release (A, Held, Status); Check (Status = C.Available);
      end;
      Check (C.Reclamation_Metrics (A).Completed > 0 and then C.Reclamation_Metrics (A).Freed_Objects > 0
        and then C.Reclamation_Metrics (A).Rejected = 0);
   end loop;
   Ada.Text_IO.Put_Line ("Resource collecting checks" & Checks'Image);
end Resource_Collecting_Tests;
