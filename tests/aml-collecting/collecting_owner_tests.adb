with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Collecting_Owner_Tests is
   procedure Clock (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Read_Microseconds => Clock, Max_Retained_Roots => 3);
   package C is new N.Owned.Collecting;
   use type Integer_Value;
   use type C.Access_Status;
   use type C.Phase;
   use type C.Value_Kind;
   use type C.Value_Handle;
   use type N.Load_Status;
   use type AML_Execute.Execution_Status;
   A, Other : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 36);
   Args : C.Arguments := [others => (C.Immediate_Argument, 0)];
   Outcome : C.Result;
   Status : C.Access_Status;
   Load_Status : N.Load_Status;
   Report : N.Initialization_Report;
   Root, Child, Copy, Extra : C.Value_Handle := C.No_Value;
   Description : C.Value_Description;
   Checks : Natural := 0;
   In_Clock : Boolean := False;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   procedure Clock (Value : out Integer_Value; Available : out Boolean) is
      S : C.Access_Status;
      R : C.Result;
      D : C.Value_Description;
   begin
      Value := 1; Available := True; In_Clock := True;
      C.Reset (A, S); Check (S = C.Busy);
      C.Release (A, Root, S); Check (S = C.Busy);
      C.Describe (A, Root, D, S); Check (S = C.Busy);
      C.Invoke (A, Input, 1, Args, 0, 100, R, S); Check (S = C.Busy and R.Charged = 0);
   end Clock;
   function Method (Name : String; Count : Byte; Code : Bytes) return Bytes is
      Head : Bytes (1 .. 7) := [16#14#, Byte (6 + Code'Length), 0, 0, 0, 0, Count];
   begin
      for I in 1 .. 4 loop Head (I + 2) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Head & Code;
   end Method;
   Program : constant Bytes :=
     Method ("BUFF", 0, [16#A4#,16#11#,6,16#0A#,3,16#41#,16#42#,16#43#]) &
     Method ("PACK", 0, [16#A4#,16#12#,10,2,16#11#,6,16#0A#,3,16#41#,16#42#,16#43#,1]) &
     Method ("ECHO", 1, [16#A4#,16#68#]) &
     Method ("TIME", 0, [16#A4#,16#5B#,16#33#]) &
     Method ("STAL", 0, [16#08#,84,69,77,80,1,16#A4#,16#71#,84,69,77,80]) &
     Method ("LIVE", 0, [16#A4#,16#71#,71,76,79,66]) &
     Bytes'[16#08#,71,76,79,66,1] &
     Method ("EMPT", 0, [16#A4#,16#12#,2,1]) &
     Method ("BOVF", 0, [16#A4#,16#5B#,16#28#,16#0A#,16#FA#,0]);
   procedure Setup (Target : in out C.Arena; Width : Integer_Width) is
   begin
      C.Reset (Target, Status); Check (Status = C.Available and C.Current (Target) = C.Loading);
      C.Load (Target, Program, Width, Load_Status, Status);
      Check (Status = C.Available and Load_Status = N.Loaded);
      C.Seal (Target, Report, Status); Check (Status = C.Available and C.Current (Target) = C.Ready);
   end Setup;
   procedure Run (Node : N.Node_ID; Count : C.Argument_Count := 0) is
   begin C.Invoke (A, Input, Node, Args, Count, 1000, Outcome, Status); end Run;
begin
   Check (C.Current (A) = C.Uninitialized);
   Run (1); Check (Status = C.Wrong_Phase and Outcome.Charged = 0);
   C.Initialize (A, Status); Check (Status = C.Available);
   C.Initialize (A, Status); Check (Status = C.Wrong_Phase);
   for Width in Integer_Width loop
      Setup (A, Width); Setup (Other, Width);
      Check (C.Node_Count (A) = 9 and C.Present (A, 4) and not C.Present (A, 31));
      C.Load (A, Program, Width, Load_Status, Status); Check (Status = C.Wrong_Phase);
      Run (31); Check (Status = C.Available and Outcome.Status = AML_Execute.Invalid_Method);
      Run (9); Check (Status = C.Available and then Outcome.Status = AML_Execute.Numeric_Overflow
        and then Outcome.Charged > 0 and then C.Retained_Count (A) = 0);
      Run (1); Check (Status = C.Available and Outcome.Status = AML_Execute.Object_Returned);
      Root := Outcome.Handle; Copy := Root;
      C.Describe (A, Root, Description, Status);
      Check (Status = C.Available and Description.Kind = C.Buffer_Description and Description.Length = 3);
      declare Data : Bytes (Natural'Last - 3 .. Natural'Last); Copied : Natural; begin
         C.Read_Bytes (A, Root, 0, Data, Copied, Status);
         Check (Status = C.Available and Copied = 3 and Data = Bytes'[16#41#,16#42#,16#43#,0]);
         C.Read_Bytes (A, Root, Natural'Last, Data, Copied, Status);
         Check (Status = C.Out_Of_Bounds and Copied = 0 and Data = Bytes'[0,0,0,0]);
      end;
      C.Describe (Other, Root, Description, Status); Check (Status = C.Invalid_Value);
      Args (0) := (C.Retained_Argument, Root);
      Run (3, 1); Check (Status = C.Available and Outcome.Status = AML_Execute.Object_Returned);
      Extra := Outcome.Handle;
      C.Release (A, Extra, Status); Check (Status = C.Available);
      Run (4); Check (Status = C.Available and Outcome.Status = AML_Execute.Returned and In_Clock);
      C.Release (A, Root, Status); Check (Status = C.Available and Root = C.No_Value);
      C.Release (A, Copy, Status); Check (Status = C.Invalid_Value);
      Run (3, 1); Check (Status = C.Invalid_Value and Outcome.Charged = 0);
      Args := [others => (C.Immediate_Argument, 0)];
      Run (2); Check (Status = C.Available and Outcome.Status = AML_Execute.Object_Returned);
      Root := Outcome.Handle;
      C.Read_Element (A, Root, 0, Child, Status); Check (Status = C.Available);
      C.Read_Element (A, Root, 1, Extra, Status); Check (Status = C.Available);
      Run (1); Check (Status = C.Root_Limit and Outcome.Charged = 0);
      C.Describe (A, Extra, Description, Status);
      Check (Status = C.Available and Description.Kind = C.Integer_Description and Description.Number = 1);
      C.Release (A, Root, Status); Check (Status = C.Available);
      C.Describe (A, Child, Description, Status);
      Check (Status = C.Available and Description.Kind = C.Buffer_Description);
      C.Dereference (A, Child, Root, Status); Check (Status = C.Wrong_Kind and Root = C.No_Value);
      C.Release (A, Child, Status); Check (Status = C.Available);
      C.Release (A, Extra, Status); Check (Status = C.Available);
      Run (5); Check (Status = C.Available and Outcome.Status = AML_Execute.Reference_Returned);
      Root := Outcome.Handle;
      C.Describe (A, Root, Description, Status);
      Check (Status = C.Available and Description.Kind = C.Reference_Description);
      C.Dereference (A, Root, Child, Status); Check (Status = C.Invalid_Value and Child = C.No_Value);
      Run (5); Check (Status = C.Available and Outcome.Status = AML_Execute.Reference_Returned);
      Extra := Outcome.Handle;
      C.Dereference (A, Root, Child, Status); Check (Status = C.Invalid_Value);
      C.Release (A, Root, Status); Check (Status = C.Available);
      C.Release (A, Extra, Status); Check (Status = C.Available);
      Run (6); Check (Status = C.Available and Outcome.Status = AML_Execute.Reference_Returned);
      Root := Outcome.Handle;
      C.Dereference (A, Root, Child, Status); Check (Status = C.Available);
      C.Release (A, Root, Status); Check (Status = C.Available);
      C.Describe (A, Child, Description, Status);
      Check (Status = C.Available and Description.Kind = C.Integer_Description and Description.Number = 1);
      Copy := Child;
      C.Release (A, Child, Status); Check (Status = C.Available);
      Run (8); Check (Status = C.Available and Outcome.Status = AML_Execute.Object_Returned);
      Extra := Outcome.Handle;
      C.Read_Element (A, Extra, 0, Child, Status);
      Check (Status = C.Uninitialized_Element and Child = C.No_Value);
      C.Read_Element (A, Extra, Natural'Last, Child, Status);
      Check (Status = C.Out_Of_Bounds and Child = C.No_Value);
      Copy := Extra;
      C.Reset (A, Status); Check (Status = C.Available and C.Retained_Count (A) = 0);
      Setup (A, Width);
      C.Describe (A, Copy, Description, Status); Check (Status = C.Invalid_Value);
      C.Release (A, Extra, Status); Check (Status = C.Invalid_Value);
   end loop;
   Ada.Text_IO.Put_Line ("COLLECTING OWNER" & Checks'Image);
end Collecting_Owner_Tests;
