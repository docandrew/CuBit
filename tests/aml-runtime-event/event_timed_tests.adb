with Ada.Real_Time;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Events;
with AML_Execute;
with AML_Integers;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Event_Timed_Tests is
   use type Ada.Real_Time.Time;
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type AML_Execute.Execution_Status;
   Calls : Natural := 0;
   Last_Elapsed : Boolean := False;
   procedure Quiescent_Wait (Milliseconds : AML_Events.Timeout; Outcome : out AML_Events.Timeout_Outcome) is
      Deadline : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (Integer (Milliseconds));
   begin
      -- This single-threaded fixture has no concurrent event delivery or AML
      -- reentry. The callback reports an elapsed deadline, never a signal.
      Calls := Calls + 1;
      Last_Elapsed := False;
      if Milliseconds in AML_Events.Poll_Only | AML_Events.Wait_Forever then
         Outcome := AML_Events.Unavailable; return;
      end if;
      delay until Deadline;
      Last_Elapsed := Ada.Real_Time.Clock >= Deadline;
      Outcome := (if Last_Elapsed then AML_Events.Elapsed else AML_Events.Failed);
   end Quiescent_Wait;
   package N is new AML_Namespace (Capacity => 16,
     Perform_Delay => AML_Delays.Unavailable_Provider, Wait_Event_Timeout => Quiescent_Wait);
   use type N.Load_Status;
   A : N.Owned.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Result : AML_Execute.Execution_Result;
   Loaded : N.Load_Status;
   Good : Boolean;
   Checks : Natural := 0;
   Wait_Opcode : constant Bytes := [16#5B#,16#25#];
   Code : constant Bytes := [16#5B#,16#02#] & Enc ("EV00")
     & Method ("TIME", [16#A4#] & Wait_Opcode & Enc ("EV00") & [1])
     & Method ("POLL", [16#A4#] & Wait_Opcode & Enc ("EV00") & [0])
     & Method ("FORE", [16#A4#] & Wait_Opcode & Enc ("EV00") & [16#0B#,16#FF#,16#FF#]);
   procedure Check (Value : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Message; end if; end Check;
   procedure Invoke (Name : String) is
      Node : constant N.Node_ID := N.Child (N.Owned.Snapshot (A), N.Root, Name);
   begin
      Check (Node /= N.Root, "method");
      N.Owned.Invoke (A, Input, Node, [others => (AML_Execute.Integer_Datum, 0, Ordinary_Integer)], 0, 1000, Result);
   end Invoke;
begin
   for Width in Integer_Width loop
      N.Owned.Reset (A, Good); Check (Good, "reset");
      N.Owned.Load (A, Code, Width, Loaded); Check (Loaded = N.Loaded, "load");
      for Repeat in 1 .. 2 loop
         declare Prior : constant Natural := Calls; begin
            Invoke ("TIME");
            Check (Result.Status = AML_Execute.Returned and then Result.Value = AML_Integers.Normalize (Integer_Value'Last, Width), "real timed timeout result");
            Check (Calls = Prior + 1 and then Last_Elapsed, "actual monotonic deadline elapsed");
            Invoke ("POLL"); Check (Result.Status = AML_Execute.Returned, "poll result");
            Check (Calls = Prior + 1, "poll bypasses provider");
            Invoke ("FORE"); Check (Result.Status = AML_Execute.Unsupported, "forever remains unavailable");
            Check (Calls = Prior + 1, "forever never sent to finite provider");
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Event timed checks" & Checks'Image);
end Event_Timed_Tests;
