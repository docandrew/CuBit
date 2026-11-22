with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_References;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Event_Named_Copy_Tests is
   package E renames AML_Execute;
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type Bytes;
   use type Integer_Value;
   use type E.Execution_Status;
   use type E.Datum_Kind;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type N.State;
   use type C.Access_Status;
   use type C.Value_Kind;
   use type C.Collection_Count;
   Checks : Natural := 0;
   Input : aliased AML_Table_Backing.State (1, 1);
   Event_Opcode : constant Bytes := [16#5B#,16#02#];
   Signal_Opcode : constant Bytes := [16#5B#,16#24#];
   Wait_Opcode : constant Bytes := [16#5B#,16#25#];
   Root_Name : constant Bytes := [16#5C#,0];
   Code : constant Bytes := Event_Opcode & Enc ("EV00") & Event_Opcode & Enc ("EV01") &
     Method ("GET0", Signal_Opcode & Enc ("EV00") & [16#A4#] & Enc ("EV00")) &
     Method ("COPY", [16#9D#] & Enc ("EV00") & Enc ("EV01") & [16#A4#] & Enc ("EV01")) &
     Method ("MAKE", Event_Opcode & Enc ("TEMP") & [16#A4#] & Enc ("TEMP")) &
     Method ("BADR", [16#9D#] & Enc ("EV00") & Root_Name & [16#A4#,0]) &
     Method ("RROT", [16#A4#,16#71#] & Root_Name) &
     Block ([16#14#], Enc ("FAIL") & [1,16#9D#] & Enc ("EV00") & [16#68#,16#A4#,0]) &
     Block ([16#14#], Enc ("POLL") & [1,16#A4#] & Wait_Opcode & [16#68#,0]);
   procedure Check (Good : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label_Text; end if; end Check;
begin
   for Width in Integer_Width loop
      -- Public owned API isolates attachment rollback from collection. The
      -- complete snapshot comparison executes in both strict profiles.
      declare
         A : N.Owned.Arena;
         OK : Boolean;
         Loaded : N.Load_Status;
         Located : N.Lookup_Result;
         Source : AML_References.Object_Handle;
         Item, Copied : E.Datum;
         Raw : E.Execution_Result;
         Status : E.Execution_Status;
      begin
         N.Owned.Reset (A, OK); Check (OK, "owned reset");
         N.Owned.Load (A, Code, Width, Loaded); Check (Loaded = N.Loaded, "owned load");
         Located := N.Resolve (N.Owned.Snapshot (A), N.Root, Path ("GET0"));
         Check (Located.Status = N.Found, "source Event method");
         N.Owned.Invoke (A, Input, Located.Node, [others => (E.Integer_Datum, 0, Ordinary_Integer)], 0, 1000, Raw);
         Check (Raw.Status = E.Object_Returned, "source Event returned by real method");
         Source := Raw.Object.Source;
         Check (N.Owned.Has_Source (A, Source), "authenticate Event source");
         N.Owned.Read_Source (A, Source, Item, Status);
         Check (Status = E.Returned and then Item.Value_Kind = E.Object_Datum, "Event object datum");
         declare Before : constant N.State := N.Owned.Snapshot (A); begin
            N.Owned.Copy_And_Attach (A, (E.Named_Destination, N.Root, Path (Root_Name)),
              Width, Item, Copied, Status);
            Check (Status = E.Unsupported_Value and then Copied.Value_Kind = E.Integer_Datum
              and then Copied.Number = 0, "root Event attachment rejected");
            Check (N.Owned.Snapshot (A) = Before, "root rejection exact tree and allocation rollback");
         end;
      end;
      declare
         A : C.Arena;
         Outcome : C.Result;
         Accessed : C.Access_Status;
         Loaded : N.Load_Status;
         Members : N.Initialization_Report;
         Original, Copy, Garbage, Root_Handle : C.Value_Handle;
         Description : C.Value_Description;
         Metrics : C.Collection_Statistics;
         Before_Usage : C.Usage_Description;
         procedure Invoke (Part : String; With_Argument : Boolean := False;
                           Argument : C.Value_Handle := C.No_Value) is
            Node : N.Node_ID;
            Args : C.Arguments := [others => (C.Immediate_Argument, 0)];
         begin
            C.Find_Child (A, N.Root, Part, Node, Accessed);
            Check (Accessed = C.Available and then Node /= N.Root, "method lookup");
            if With_Argument then Args (0) := (C.Retained_Argument, Argument); end if;
            C.Invoke (A, Input, Node, Args, (if With_Argument then 1 else 0), 1000, Outcome, Accessed);
            Check (Accessed = C.Available, "invocation admission");
         end Invoke;
         procedure Release (Value : in out C.Value_Handle) is
         begin C.Release (A, Value, Accessed); Check (Accessed = C.Available, "release result"); end Release;
      begin
         C.Initialize (A, Accessed); Check (Accessed = C.Available, "initialize");
         C.Load (A, Code, Width, Loaded, Accessed);
         Check (Accessed = C.Available and then Loaded = N.Loaded, "collecting load");
         C.Seal (A, Members, Accessed); Check (Accessed = C.Available, "seal");
         Invoke ("GET0"); Check (Outcome.Status = E.Object_Returned, "retain signaled original");
         Original := Outcome.Handle;
         Invoke ("COPY"); Check (Outcome.Status = E.Object_Returned, "named Event copy");
         Copy := Outcome.Handle;
         C.Describe (A, Copy, Description, Accessed);
         Check (Accessed = C.Available and then Description.Kind = C.Event_Description, "copy remains Event");
         Invoke ("POLL", True, Copy);
         Check (Outcome.Status = E.Returned and then Outcome.Number =
           (if Width = Bits_32 then 16#FFFF_FFFF# else 16#FFFF_FFFF_FFFF_FFFF#), "fresh copy is unsignaled");
         Invoke ("POLL", True, Original);
         Check (Outcome.Status = E.Returned and then Outcome.Number = 0, "original signal independent of copy");
         Release (Copy);
         Metrics := C.Reclamation_Metrics (A);
         Invoke ("BADR");
         Check (Outcome.Status = E.Bad_Name and then
           C.Reclamation_Metrics (A).Attempted = Metrics.Attempted,
           "direct zero-part root rejected before copy callback: " & Outcome.Status'Image);
         Invoke ("RROT"); Check (Outcome.Status = E.Reference_Returned, "real retained RefOf root");
         Root_Handle := Outcome.Handle;
         Invoke ("MAKE"); Check (Outcome.Status = E.Object_Returned, "create collectible Event");
         Garbage := Outcome.Handle; Release (Garbage);
         Metrics := C.Reclamation_Metrics (A); Before_Usage := C.Observe_Usage (A);
         Invoke ("FAIL", True, Root_Handle);
         Check (Outcome.Status = E.Unsupported_Value,
           "root rejection through collecting hook: " & Outcome.Status'Image);
         Check (C.Reclamation_Metrics (A).Completed > Metrics.Completed
           and then C.Reclamation_Metrics (A).Freed_Objects > Metrics.Freed_Objects,
           "failed attachment preserves earlier actual reclamation");
         Check (C.Reclamation_Metrics (A).Rejected = Metrics.Rejected, "authenticated collection census");
         Check (C.Observe_Usage (A).Nodes = Before_Usage.Nodes
           and then C.Observe_Usage (A).Values.Objects < Before_Usage.Values.Objects,
           "rejected root clone unpublished; reclaimed garbage stays reclaimed");
         Release (Root_Handle);
         C.Describe (A, Original, Description, Accessed);
         Check (Accessed = C.Available and then Description.Kind = C.Event_Description
           and then C.Retained_Count (A) = 1, "prior retained original survives failure and GC");
         Invoke ("POLL", True, Original);
         Check (Outcome.Status = E.Returned and then Outcome.Number =
           (if Width = Bits_32 then 16#FFFF_FFFF# else 16#FFFF_FFFF_FFFF_FFFF#), "original consumed state preserved");
         Release (Original); Check (C.Retained_Count (A) = 0, "final root cleanup");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("named Event copy checks" & Checks'Image);
end Event_Named_Copy_Tests;
