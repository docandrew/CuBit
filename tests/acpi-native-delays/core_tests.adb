pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with ACPI_Service;
with ACPI_Native_Delays;
with AML_Execute;
with AML_Decode;
with AML_Names;
with Firmware_Tables;
with Delay_Model;
procedure Core_Tests is
   package S renames ACPI_Service;
   package M renames Delay_Model;
   package D renames ACPI_Native_Delays;
   package IO renames Ada.Streams.Stream_IO;
   use type Ada.Streams.Stream_Element_Offset;
   use type S.Install_Status, S.Values.Access_Status;
   use type AML_Execute.Execution_Status;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in 36 .. 4096, "bounded actual AML fixture");
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "whole compiled fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Data : constant Firmware_Tables.Bytes := Read_Table;
   Tree : aliased S.State (1, Data'Length, Data'Length, 1);
   Installed : S.Install_Status;
   Initialization : S.Namespace.Initialization_Report;
   Access_Result : S.Values.Access_Status;
   Accepted : Boolean;
   Deadline : Unsigned_64 := 10_000_000;
   procedure Invoke (Name : AML_Names.Segment; Expected : AML_Execute.Execution_Status;
      Number : AML_Decode.Integer_Value := 0; Arg_Count : Natural := 0;
      Arg : AML_Decode.Integer_Value := 0) is
      Node : S.Namespace.Node_ID;
      Result : S.Values.Result;
      Scalar : AML_Execute.Execution_Result;
      Args : AML_Execute.Arguments := [others => 0];
      Before_Reads, Before_Yields, Before_Sleeps : Natural;
   begin
      Args (0) := Arg;
      S.Find_Child (Tree, S.Namespace.Root, Name, Node, Access_Result);
      Check (Access_Result = S.Values.Available and Node /= S.Namespace.Root, "actual named method found");
      Before_Reads := M.Reads; Before_Yields := M.Yields; Before_Sleeps := M.Sleeps;
      S.Invoke_Retained (Tree, Node, Args, Arg_Count, 1_000, Result, Access_Result);
      Check (Access_Result = S.Values.Available and Result.Status = Expected, "retained native provider propagation " & Result.Status'Image);
      if Expected = AML_Execute.Returned then Check (Result.Number = Number, "retained returned scalar matches ASL"); end if;
      Check (S.Retained_Results (Tree) = 0 and S.Valid (Tree), "delay leaves no retained integer result or invalid arena");
      if Name = "STAL" then Check (M.Yields = Before_Yields and M.Sleeps = Before_Sleeps, "real interpreter Stall does not relinquish"); end if;
      if Expected = AML_Execute.Invalid_Delay or Mode = "closed" then
         Check (M.Reads = Before_Reads and M.Yields = Before_Yields and M.Sleeps = Before_Sleeps,
           "invalid/closed request does not reach native work");
      end if;
      Before_Reads := M.Reads; Before_Yields := M.Yields; Before_Sleeps := M.Sleeps;
      S.Invoke_Scalar (Tree, Node, Args, Arg_Count, 1_000, Scalar);
      Check (Scalar.Status = (if Expected = AML_Execute.Delay_Failed then AML_Execute.Unsupported else Expected),
        "legacy scalar adapter preserves one-shot failed native domain");
      if Expected = AML_Execute.Returned then Check (Scalar.Value = Number, "scalar returned value matches ASL");
      else Check (M.Reads = Before_Reads and M.Yields = Before_Yields and M.Sleeps = Before_Sleeps,
        "failed or invalid operation never replenishes native work on next invocation");
      end if;
   end Invoke;
begin
   if Mode = "unavailable" then M.Available := False;
   elsif Mode = "stopped" then M.Stopped := True;
   elsif Mode = "deadline" then Deadline := 999;
   end if;
   if Mode /= "closed" then D.Begin_Startup (Deadline, Accepted); Check (Accepted, "startup domain opened"); end if;
   S.Install (Tree, 1, S.DSDT, Data, Installed); Check (Installed = S.Installed, "actual table installed");
   S.Initialize_Members (Tree, Initialization, Access_Result);
   Check (Access_Result = S.Values.Available, "actual namespace initialized");
   if Mode = "normal" then
      Invoke ("STAL", AML_Execute.Returned, 18);
      Invoke ("SLEP", AML_Execute.Returned, 19);
      Invoke ("ZDEL", AML_Execute.Returned, 17);
      Invoke ("BOTH", AML_Execute.Returned, 20);
      Invoke ("BAD0", AML_Execute.Invalid_Delay, Arg_Count => 1, Arg => 256);
      Invoke ("STAL", AML_Execute.Returned, 18);
   elsif Mode in "closed" | "unavailable" then Invoke ("STAL", AML_Execute.Unsupported);
   elsif Mode = "stopped" then Invoke ("STAL", AML_Execute.Delay_Failed);
   elsif Mode = "deadline" then Invoke ("SLEP", AML_Execute.Delay_Failed);
   else raise Program_Error with "unknown test mode";
   end if;
   D.Close_Startup;
   Invoke ("STAL", AML_Execute.Unsupported);
   D.Begin_Startup (Unsigned_64'Last, Accepted); Check (not Accepted, "same startup cannot replenish elapsed-time budget");
   Ada.Text_IO.Put_Line ("Native delay Core checks" & Checks'Image);
end Core_Tests;
