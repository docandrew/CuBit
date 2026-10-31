pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with AML_Table_Backing;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Ready_Module_Append_Tests is
   package C renames ACPI_Service;
   package V renames C.Values;
   package N renames C.Namespace;
   use type V.Access_Status, V.Phase, V.Module_Outcome, V.Preparation_Outcome;
   use type V.Append_Outcome, V.Value_Handle;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type Firmware_Tables.Bytes, Firmware_Tables.Byte;
   use type Ada.Streams.Stream_Element_Offset;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File); Check (Last = Raw'Last, "whole copied fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Added : constant Firmware_Tables.Bytes := Read_Table (2);
   Mode : constant String := Ada.Command_Line.Argument (3);
   Width : constant AML_Decode.Integer_Width := (if Boot (9) < 2 then AML_Decode.Bits_32 else AML_Decode.Bits_64);
   Input : aliased AML_Table_Backing.State (2, Boot'Length + Added'Length);
   Arena, Foreign_Arena, Scope_Source : V.Arena;
   Selected_Mode : constant Boolean := Mode in "scope" | "foreign-scope" | "stale-scope";
   Selected_Node : N.Node_ID;
   Selected_At, Invalid_At, Stale_At : V.Node_Descriptor := V.No_Node;
   Attempt : V.Append_Attempt;
   Report : V.Append_Report;
   Status : V.Access_Status;
   Prepared : V.Preparation_Outcome;
   Modules : V.Module_Report;
   Initial : N.Initialization_Report;
   Result : V.Result;
   Held : V.Value_Handle;
   Saved_Held : V.Value_Handle;
   Counter, Getter, Current_Method : N.Node_ID;
   Before_Node : V.Node_Descriptor;
   Args : constant V.Arguments := [others => (V.Immediate_Argument, 0)];
   Data : AML_Decode.Bytes (1 .. 3);
   Copied : Natural;
   procedure Find (Name : String; Node : out N.Node_ID; Parent : N.Node_ID := N.Root) is
   begin
      V.Find_Child (Arena, Parent, Name, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "find current " & Name);
   end Find;
   procedure Observe_Counter (Expected : AML_Decode.Integer_Value) is
   begin
      V.Observe_Named_Value (Arena, Counter, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned
        and then Result.Number = Expected, "committed counter visible without executing AML");
   end Observe_Counter;
   procedure Append (Budget : Natural; Index : Positive := 2; Root_At : V.Module_Root := V.Namespace_Module_Root) is
   begin
      V.Append_Ready_Module (Arena, Input, Index, Width, Budget, Attempt, Report, Status, Root_At);
      Check (V.Valid (Arena) and then Report.Charged <= Budget, "append validity and budget");
   end Append;
   procedure Initialize (Owner : in out V.Arena) is
   begin
   V.Reset (Owner, Status); Check (Status = V.Available, "initialize real owned arena");
   V.Prepare_Firmware_Namespace (Owner, Prepared, Status);
   Check (Status = V.Available and then Prepared = V.Firmware_Prepared, "seed real firmware namespace");
   V.Execute_Modules (Owner, Input, 10000, Modules, Status);
   Check (Status = V.Available and then Modules.Outcome = V.Modules_Completed
     and then Modules.Completed = 1, "boot executes only DSDT and leaves OEM block retained");
   V.Seal_With_Tables (Owner, Input, Initial, Status);
   Check (Status = V.Available and then V.Current (Owner) = V.Ready, "ready namespace before append");
   end Initialize;

begin
   Input.Count := 2;
   Input.Tables (1) := (0, Boot'Length); Input.Tables (2) := (Boot'Length, Added'Length);
   Input.Data.Bytes := Boot & Added;
   Append (10000);
   Check (Status = V.Wrong_Phase and then not V.Consumed (Attempt), "uninitialized owner cannot consume attempt");
   if Mode = "stale-scope" then
      Initialize (Arena);
      Find ("DEV0", Selected_Node);
      V.Describe_Node (Arena, Selected_Node, Stale_At, Status);
      Check (Status = V.Available, "capture earlier scope generation");
   end if;
   Initialize (Arena);
   if Selected_Mode then
      Find ("DEV0", Selected_Node);
      V.Describe_Node (Arena, Selected_Node, Selected_At, Status);
      Check (Status = V.Available, "capture current selected scope identity");
   end if;
   if Mode = "explicit-root" then
      V.Describe_Node (Arena, N.Root, Selected_At, Status);
      Check (Status = V.Available, "capture explicit current namespace root descriptor");
   end if;
   Find ("CNT0", Counter); Find ("GET0", Getter); Observe_Counter (0);
   V.Describe_Node (Arena, Counter, Before_Node, Status); Check (Status = V.Available, "old node identity captured");
   V.Invoke (Arena, Input, Getter, Args, 0, 10000, Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "retain old buffer across module append");
   Held := Result.Handle;
   Check (V.Retained_Count (Arena) = 1, "one pre-existing caller pin");
   if Mode = "zero" then
      Append (0);
      Check (Status = V.Available and then Report.Outcome = V.Append_Budget_Refused
        and then not V.Consumed (Attempt) and then V.Current (Arena) = V.Ready,
        "zero fuel refuses without poisoning or consuming owner");
      Observe_Counter (0);
   elsif Mode = "invalid" then
      Append (10000, 3);
      Check (Status = V.Invalid_Value and then not V.Consumed (Attempt)
        and then V.Current (Arena) = V.Ready, "out-of-catalog source refuses without effects");
      Observe_Counter (0);
   end if;
   declare
      Checksum_Index : constant Positive := Boot'Length + 10;
      Saved : constant Firmware_Tables.Byte := Input.Data.Bytes (Checksum_Index);
   begin
      Input.Data.Bytes (Checksum_Index) := Saved + 1;
      Append (10000);
      Check (Status = V.Invalid_Value and then not V.Consumed (Attempt)
        and then V.Current (Arena) = V.Ready, "bad checksum refuses before effects");
      Input.Data.Bytes (Checksum_Index) := Saved;
   end;
   Input.Tables (2).Extent := Firmware_Tables.Table_Header_Size - 1;
   Append (10000);
   Check (Status = V.Invalid_Value and then not V.Consumed (Attempt), "short header refuses before indexing");
   Input.Tables (2).Extent := Added'Length;
   Append (10000, 1);
   Check (Status = V.Invalid_Value and then not V.Consumed (Attempt), "boot DSDT cannot append twice");
   V.Configure_Notifications (Arena, True, Status);
   Check (Status = V.Available, "enable observer capture before refusal");
   Append (10000);
   Check (Status = V.Invalid_Value and then not V.Consumed (Attempt), "enabled observer requires future readiness integration");
   V.Configure_Notifications (Arena, False, Status);
   Check (Status = V.Available, "disable observer for internal append prerequisite");
   Observe_Counter (0);
   if Selected_Mode then
      Append (10000, Root_At => (V.Selected_Root, V.No_Node));
      Check (Status = V.Invalid_Value and then not V.Consumed (Attempt)
        and then V.Current (Arena) = V.Ready, "missing selected scope cannot default to namespace root");
      V.Describe_Node (Arena, Counter, Invalid_At, Status);
      Check (Status = V.Available, "capture data object as invalid lexical scope");
      Append (10000, Root_At => (V.Selected_Root, Invalid_At));
      Check (Status = V.Invalid_Value and then not V.Consumed (Attempt)
        and then V.Current (Arena) = V.Ready, "non-scope target refuses before execution");
      if Mode = "foreign-scope" then
         Initialize (Scope_Source);
         V.Find_Child (Scope_Source, N.Root, "DEV0", Current_Method, Status);
         Check (Status = V.Available, "find foreign owner scope at matching name");
         V.Describe_Node (Scope_Source, Current_Method, Invalid_At, Status);
         Check (Status = V.Available, "capture foreign owner scope descriptor");
         Append (10000, Root_At => (V.Selected_Root, Invalid_At));
         Check (Status = V.Invalid_Value and then not V.Consumed (Attempt)
           and then V.Current (Arena) = V.Ready, "foreign scope descriptor cannot select a current owner node");
      elsif Mode = "stale-scope" then
         Append (10000, Root_At => (V.Selected_Root, Stale_At));
         Check (Status = V.Invalid_Value and then not V.Consumed (Attempt)
           and then V.Current (Arena) = V.Ready, "stale generation cannot select a replacement scope");
      end if;
      Observe_Counter (0);
   end if;
   Append (10000, Root_At => (if Selected_Mode or else Mode = "explicit-root"
     then (V.Selected_Root, Selected_At) else V.Namespace_Module_Root));
   Check (Status = V.Available and then V.Consumed (Attempt), "admitted attempt consumed once");
   Check (V.Retained_Count (Arena) = 1, "old retained root survives");
   Check ((if Mode in "fail" | "fatal" then not V.Current_Node (Arena, Before_Node)
     else V.Current_Node (Arena, Before_Node)), "node descriptor available only in ready state");
   V.Read_Bytes (Arena, Held, 0, Data, Copied, Status);
   Check ((if Mode in "fail" | "fatal" then Status = V.Wrong_Phase and then Copied = 0
     else Status = V.Available and then Copied = 3 and then Data = [1,2,3]),
     "old buffer readable after success and refused during recovery");
   Observe_Counter (1);
   if Mode in "fail" | "fatal" then
      Check (Report.Outcome = V.Append_Failed and then Report.Recovery_Needed
        and then V.Current (Arena) = V.Loading, "failed module blocks later AML without rolling back committed effects");
      Check (Report.Failure = (if Mode = "fatal" then AML_Execute.Fatal_Requested else AML_Execute.Division_By_Zero),
        "typed module failure preserved");
      V.Invoke (Arena, Input, Getter, Args, 0, 10000, Result, Status);
      Check (Status = V.Wrong_Phase, "failed append cannot serve another invocation");
   else
      Check (Report.Outcome = V.Append_Completed and then not Report.Recovery_Needed
        and then V.Current (Arena) = V.Ready, "successful append restores ready owner");
      Find ("NVAL", Current_Method, (if Selected_Mode then Selected_Node else N.Root));
      if Selected_Mode then
         declare Absent : N.Node_ID; begin
            V.Find_Child (Arena, N.Root, "NVAL", Absent, Status);
            Check (Status = V.Available and then Absent = N.Root, "relative method stays beneath selected root");
         end;
         Check (V.Current_Node (Arena, Selected_At), "selected parent identity survives persistent declarations");
      end if;
      V.Invoke (Arena, Input, Current_Method, Args, 0, 10000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned
        and then Result.Number = 42, "new persistent method executes in real namespace");
   end if;
   Append (10000);
   Check (Status = V.Wrong_Phase and then Report.Outcome = V.Append_Already_Consumed,
     "same attempt never replays body after success or failure");
   Observe_Counter (1);
   V.Append_Ready_Module (Foreign_Arena, Input, 2, Width, 10000, Attempt, Report, Status);
   Check (Status = V.Wrong_Phase and then Report.Outcome = V.Append_Already_Consumed
     and then V.Current (Foreign_Arena) = V.Uninitialized, "consumed attempt cannot replay on another arena");
   Saved_Held := Held;
   V.Release (Arena, Held, Status);
   if Mode in "fail" | "fatal" then
      Check (Status = V.Available and then Held = V.No_Value and then V.Retained_Count (Arena) = 0,
        "terminal recovery allows authenticated pin cleanup");
      V.Read_Bytes (Arena, Saved_Held, 0, Data, Copied, Status);
      Check (Status = V.Wrong_Phase and then Copied = 0 and then V.Current (Arena) = V.Loading,
        "pin cleanup leaves retained reads closed");
      V.Release (Arena, Saved_Held, Status);
      Check (Status = V.Invalid_Value and then Saved_Held /= V.No_Value and then V.Retained_Count (Arena) = 0,
        "duplicate release cannot decrement twice");
      V.Reset (Arena, Status);
      Check (Status = V.Available and then V.Retained_Count (Arena) = 0,
        "explicit owner reset disposes failed namespace and retained roots");
      V.Read_Bytes (Arena, Saved_Held, 0, Data, Copied, Status);
      Check (Status = V.Wrong_Phase and then Copied = 0, "old handle cannot read reset owner");
      Check (V.Consumed (Attempt), "owner reset cannot revive consumed load attempt");
   else
      Check (Status = V.Available and then Held = V.No_Value and then V.Retained_Count (Arena) = 0,
        "caller pin releases after successful append");
   end if;
   Check (Input.Data.Bytes = Boot & Added, "immutable table bytes unchanged");
   Ada.Text_IO.Put_Line ("Ready module append checks" & Checks'Image);
end Ready_Module_Append_Tests;
