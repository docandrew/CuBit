with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Integers;
with AML_Namespace;
with AML_Table_Backing;
with Event_Edge_Data;
procedure Event_Edge_Tests is
   package E renames AML_Execute;
   package N is new AML_Namespace (Capacity => 64, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type AML_Decode.Integer_Value;
   use type C.Access_Status;
   use type N.Load_Status;
   use type E.Execution_Status;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Init : N.Initialization_Report;
   Result : C.Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   procedure Load (Data : Bytes; Width : Integer_Width) is
   begin
      C.Reset (A, Status); Check (Status = C.Available, "reset");
      C.Load (A, Data, Width, Loaded, Status); Check (Status = C.Available and then Loaded = N.Loaded, "load");
      C.Seal (A, Init, Status); Check (Status = C.Available, "seal");
   end Load;
   procedure Invoke (Name : String; Expected : Integer_Value; Rejected : Boolean := False) is
      Node : N.Node_ID;
   begin
      C.Find_Child (A, N.Root, Name, Node, Status); Check (Status = C.Available and then Node /= N.Root, "find method");
      C.Invoke (A, Input, Node, [others => (C.Immediate_Argument, 0)], 0, 10000, Result, Status);
      Check (Status = C.Available, "invocation");
      if Rejected then Check (Result.Status = E.Unsupported_Value, "strict Event Store rejection " & Result.Status'Image);
      else
         Check (Result.Status = E.Returned, Name & " status " & Result.Status'Image);
         Check (Result.Number = Expected, Name & " expected" & Expected'Image & " actual" & Result.Number'Image);
      end if;
      Check (C.Retained_Count (A) = 0, "no retained root leak");
   end Invoke;
begin
   C.Initialize (A, Status); Check (Status = C.Available, "initialize");
   for Width in Integer_Width loop
      Load ((if Width = Bits_32 then Event_Edge_Data.Debug_1 else Event_Edge_Data.Debug_2), Width);
      for Repeat in 1 .. 2 loop Invoke ("TEST", 51); end loop;
      for Repeat in 1 .. 2 loop Invoke ("BAD0", 0, True); end loop;
   end loop;
   for Width in Integer_Width loop
      Load ((if Width = Bits_32 then Event_Edge_Data.Timing_1 else Event_Edge_Data.Timing_2), Width);
      for Repeat in 1 .. 2 loop Invoke ("BASE", 0); end loop;
      for Repeat in 1 .. 2 loop Invoke ("COPY", AML_Integers.Normalize (Integer_Value'Last, Width)); end loop;
      for Repeat in 1 .. 2 loop Invoke ("RSET", AML_Integers.Normalize (Integer_Value'Last, Width)); end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Event edge checks" & Checks'Image);
end Event_Edge_Tests;
