with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Event_Collection_Tests is
   package E renames AML_Execute;
   Capacity : constant := 12;
   package N is new AML_Namespace (Capacity => Capacity, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type E.Execution_Status;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Value_Kind;
   use type C.Audit_Value;
   use type C.Collection_Statistics;
   use type C.Collection_Count;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Outcome : C.Result;
   Accessed : C.Access_Status;
   Loaded : N.Load_Status;
   Initialized : N.Initialization_Report;
   Before : C.Audit_Value;
   Metrics : C.Collection_Statistics;
   Saved, Stale : C.Value_Handle;
   Description : C.Value_Description;
   Checks : Natural := 0;
   Event_Opcode : constant Bytes := [16#5B#, 16#02#];
   Signal_Opcode : constant Bytes := [16#5B#, 16#24#];
   Wait_Opcode : constant Bytes := [16#5B#, 16#25#];
   Code : constant Bytes := Event_Opcode & Enc ("EV00")
     & Method ("MAKE", Event_Opcode & Enc ("TEMP") & [16#A4#] & Enc ("TEMP"))
     & Block ([16#14#], Enc ("POLL") & [1] & Signal_Opcode & [16#68#,16#A4#] & Wait_Opcode & [16#68#,0])
     & Method ("FAIL", Event_Opcode & Enc ("TEMP") & [16#A4#] & Enc ("MISS"))
     & Method ("CLSH", Event_Opcode & [16#5C#] & Enc ("EV00") & [16#A4#,0]);
   procedure Check (Value : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Label_Text; end if; end Check;
   procedure Invoke (Name : String; With_Saved : Boolean := False) is
      Node : N.Node_ID;
      Args : C.Arguments := [others => (C.Immediate_Argument, 0)];
   begin
      C.Find_Child (A, N.Root, Name, Node, Accessed);
      Check (Accessed = C.Available and then Node /= N.Root, "method present");
      if With_Saved then Args (0) := (C.Retained_Argument, Saved); end if;
      C.Invoke (A, Input, Node, Args, (if With_Saved then 1 else 0), 1000, Outcome, Accessed);
      Check (Accessed = C.Available, "invoke admitted");
   end Invoke;
   procedure Release_Outcome is
   begin
      C.Release (A, Outcome.Handle, Accessed); Check (Accessed = C.Available, "release new event");
   end Release_Outcome;
begin
   C.Initialize (A, Accessed); Check (Accessed = C.Available, "initialize");
   for Width in Integer_Width loop
      C.Reset (A, Accessed); Check (Accessed = C.Available, "reset");
      C.Load (A, Code, Width, Loaded, Accessed);
      Check (Accessed = C.Available and then Loaded = N.Loaded, "load");
      C.Seal (A, Initialized, Accessed); Check (Accessed = C.Available, "seal");
      Invoke ("MAKE"); Check (Outcome.Status = E.Object_Returned, "retained escaped event");
      Saved := Outcome.Handle;
      Metrics := C.Reclamation_Metrics (A);
      for Repeat in 1 .. 4 loop
         Invoke ("MAKE"); Check (Outcome.Status = E.Object_Returned, "repeated event allocation");
         Release_Outcome;
      end loop;
      Check (C.Reclamation_Metrics (A).Completed > Metrics.Completed, "real collections occurred");
      Check (C.Reclamation_Metrics (A).Freed_Objects > Metrics.Freed_Objects, "retired events reclaimed");
      Check (C.Reclamation_Metrics (A).Rejected = Metrics.Rejected, "no rejected root census");
      C.Describe (A, Saved, Description, Accessed);
      Check (Accessed = C.Available and then Description.Kind = C.Event_Description, "old retained event survives collection");
      Invoke ("POLL", True); Check (Outcome.Status = E.Returned and then Outcome.Number = 0, "retained event remains operable");
      Before := C.Audit (A); Metrics := C.Reclamation_Metrics (A);
      Invoke ("CLSH"); Check (Outcome.Status = E.Duplicate_Name, "duplicate rejected");
      Check (C.Audit (A) = Before, "duplicate preserves exact arena");
      Check (C.Reclamation_Metrics (A) = Metrics, "duplicate does not collect");
      Metrics := C.Reclamation_Metrics (A);
      Invoke ("FAIL"); Check (Outcome.Status = E.Unknown_Name, "failure after event allocation retained");
      Check (C.Node_Count (A) = 5, "failed method event node removed");
      Check (C.Reclamation_Metrics (A).Completed > Metrics.Completed, "post-GC failure retains real metrics");
      Check (C.Reclamation_Metrics (A).Freed_Objects > Metrics.Freed_Objects, "post-GC failure does not resurrect retired event");
      Metrics := C.Reclamation_Metrics (A);
      Invoke ("MAKE"); Check (Outcome.Status = E.Object_Returned, "success after failure"); Release_Outcome;
      Check (C.Reclamation_Metrics (A).Freed_Objects > Metrics.Freed_Objects, "failed invocation event reclaimed");
      Invoke ("POLL", True); Check (Outcome.Status = E.Returned and then Outcome.Number = 0, "held event survives failed invocation");
      Stale := Saved; C.Release (A, Saved, Accessed); Check (Accessed = C.Available, "release old event");
      Invoke ("MAKE"); Check (Outcome.Status = E.Object_Returned, "collect released old event"); Release_Outcome;
      C.Describe (A, Stale, Description, Accessed); Check (Accessed /= C.Available, "stale retained identity rejected after collection");
      Check (C.Retained_Count (A) = 0, "no root leaks");
      -- A separate full namespace rejects before collecting or allocating.
      C.Reset (A, Accessed); Check (Accessed = C.Available, "reset for full namespace");
      C.Load (A, Code, Width, Loaded, Accessed); Check (Accessed = C.Available and then Loaded = N.Loaded, "reload");
      for I in 1 .. Capacity - 5 loop
         C.Load (A, [16#08#] & Enc ("N00" & Character'Val (Character'Pos ('A') + I - 1)) & [0], Width, Loaded, Accessed);
         Check (Accessed = C.Available and then Loaded = N.Loaded, "fill namespace");
      end loop;
      C.Seal (A, Initialized, Accessed); Check (Accessed = C.Available, "seal full");
      Before := C.Audit (A); Metrics := C.Reclamation_Metrics (A);
      Invoke ("MAKE"); Check (Outcome.Status = E.Namespace_Limit, "full namespace rejects event");
      Check (C.Audit (A) = Before, "full failure preserves exact arena");
      Check (C.Reclamation_Metrics (A) = Metrics, "full failure does not collect");
   end loop;
   Ada.Text_IO.Put_Line ("Event collection checks" & Checks'Image);
end Event_Collection_Tests;
