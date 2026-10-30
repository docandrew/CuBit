pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Datums;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Index_Width_Oracle;
procedure Index_Width_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 96, Retained_Result_Capacity => 16);
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Kind;
   use type C.Values.Value_Handle;
   use type AML_Execute.Execution_Status;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Request;
   use type A.Observation;
   use type A.Operation_ID;
   type Fault_Kind is (No_Fault, Denied_Before_Effect, Failed_After_Effect,
     Unknown_Before_Effect, Unknown_After_Effect);
   type Slot is (Index_Port, Byte_Data);
   type Register_Values is array (Slot) of AML_Decode.Integer_Value;
   type Register_IDs is array (Slot) of A.Register_ID;
   IDs : constant Register_IDs := [16#8000_0000_0000_0011#, 16#A000_0000_0000_0055#];
   type Request_Log is array (Positive range 1 .. 128) of A.Request;
   type Index_Trace is array (Positive range 1 .. 16) of AML_Decode.Integer_Value;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Registers : Register_Values := [others => 0];
      Index_Writes : Natural := 0;
      Trace : Index_Trace := [others => 0];
      Data_Reads, Calls : Natural := 0;
      Requests : Request_Log := [others => (others => <>)];
      Allow_Data_Read : Boolean := True;
      Fault : Fault_Kind := No_Fault;
      Fault_At : Natural range 0 .. Request_Log'Length := 0;
      Receipt, Confirmation : A.Observation := A.No_Observation;
      Next_Operation : A.Operation_ID := 1;
   end record;
   function Pending (IO : Context) return Boolean is
     (IO.Owner = AML_Identity.No_Identity or else IO.Receipt.Present);
   function Width_Of (S : Slot) return T.Access_Width is
     (case S is when Index_Port => T.Byte_Access, when Byte_Data => T.Byte_Access);
   function Offset_Of (S : Slot) return AML_Decode.Integer_Value is
     (if S = Index_Port then 0 else 1);
   procedure Locate (ID : A.Register_ID; S : out Slot; Found : out Boolean) is
   begin
      S := Index_Port; Found := False;
      for Candidate in Slot loop
         if IDs (Candidate) = ID then S := Candidate; Found := True; return; end if;
      end loop;
   end Locate;
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      S : Slot;
      Found : Boolean;
   begin
      Locate (Target.Register, S, Found);
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 16),
        Byte_Offset => Offset_Of (S), Width => Width_Of (S));
      Accepted := Found and then IO.Owner /= AML_Identity.No_Identity
        and then Target.Provider = IO.Owner and then Target.Endpoint = 7 and then Target.Epoch = 1;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then (For_Write or else Target.Register = IDs (Index_Port) or else IO.Allow_Data_Read);
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      S : Slot;
      Found, Allowed : Boolean;
      Fault : Fault_Kind;
      procedure Retain_Unknown is
      begin
         IO.Receipt := (True, IO.Owner, Item, IO.Next_Operation);
         Reply := (A.Indeterminate, IO.Owner, 0, IO.Next_Operation);
         IO.Next_Operation := IO.Next_Operation + 1;
      end Retain_Unknown;
   begin
      Reply := (others => <>);
      Locate (Item.Destination.Register, S, Found);
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Found or else not Allowed then return; end if;
      IO.Calls := IO.Calls + 1; IO.Requests (IO.Calls) := Item;
      Fault := (if IO.Calls = IO.Fault_At then IO.Fault else No_Fault);
      if Fault /= No_Fault then IO.Fault := No_Fault; end if;
      if Fault = Denied_Before_Effect then return; end if;
      if Fault = Unknown_Before_Effect then Retain_Unknown; return; end if;
      if Item.For_Write then
         if Item.Value > AML_Field_Datums.Width_Mask (Width_Of (S)) then return; end if;
         IO.Registers (S) := Item.Value;
         if S = Index_Port then
            IO.Index_Writes := IO.Index_Writes + 1;
            if IO.Index_Writes > IO.Trace'Length then raise Program_Error with "trace capacity"; end if;
            IO.Trace (IO.Index_Writes) := Item.Value;
         end if;
      elsif S /= Index_Port then IO.Data_Reads := IO.Data_Reads + 1;
      end if;
      if Fault = Failed_After_Effect then
         Reply := (A.Failed_Completed, IO.Owner, 0, A.No_Operation); return;
      end if;
      if Fault = Unknown_After_Effect then Retain_Unknown; return; end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Registers (S)), A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin
      Accepted := Expected.Present and then not Pending (IO)
        and then Expected = IO.Confirmation;
   end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   procedure Exercise (Revision : AML_Decode.Byte; Faults : Boolean := False;
     Writing : Boolean := False) is
      Raw : constant AML_Decode.Bytes := (if Revision = 1 then Index_Width_Oracle.Table32 else Index_Width_Oracle.Table64);
      Service : aliased C.State (1, Raw'Length, Raw'Length);
      IO : Context;
      Items : F.Binding_Set;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Status : C.Values.Access_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Issued : Boolean;
      procedure Expect (Name : AML_Names.Segment; Number : AML_Decode.Integer_Value := 0;
        Bytes : AML_Decode.Bytes := [1 .. 0 => 0];
        Want : AML_Execute.Execution_Status := AML_Execute.Returned) is
         Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor;
         Result : C.Values.Result;
         Description : C.Values.Value_Description;
         Handle : C.Values.Value_Handle;
         Data : AML_Decode.Bytes (1 .. Bytes'Length);
         Copied : Natural;
         Expected_Status : constant AML_Execute.Execution_Status :=
           (if Want /= AML_Execute.Returned then Want elsif Bytes'Length > 0
            then AML_Execute.Object_Returned else AML_Execute.Returned);
      begin
         IO.Calls := 0; IO.Index_Writes := 0; IO.Data_Reads := 0;
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " lookup");
         C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, Name & " descriptor");
         F.Evaluate_Named (Service, IO, Items, Item, 100_000, Result, Status);
         Check (Status = C.Values.Available and then Result.Status = Expected_Status,
           Name & " expected " & Expected_Status'Image & " got " & Result.Status'Image);
         if Expected_Status = AML_Execute.Object_Returned then
            Handle := Result.Handle;
            C.Describe_Result (Service, Handle, Description, Status);
            Check (Status = C.Values.Available and then Description.Kind = C.Values.Buffer_Description
              and then Description.Length = Bytes'Length, Name & " exact buffer length");
            C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
            Check (Status = C.Values.Available and then Copied = Data'Length, Name & " buffer copy");
            for I in Data'Range loop Check (Data (I) = Bytes (Bytes'First + I - 1), Name & " buffer byte" & I'Image); end loop;
            Check (C.Retained_Results (Service) = 1, Name & " one result");
            C.Release_Result (Service, Handle, Status);
            Check (Status = C.Values.Available and then Handle = C.Values.No_Value, Name & " release");
         elsif Expected_Status = AML_Execute.Returned then
            Check (Result.Number = Number, Name & " scalar");
         end if;
         Check (C.Retained_Results (Service) = 0, Name & " no retained roots");
      end Expect;
      procedure Request_Is (Position : Positive; S : Slot; Writing : Boolean;
        Number : AML_Decode.Integer_Value := 0) is
      begin
         Check (Position <= IO.Calls and then IO.Requests (Position) =
           A.Request'((IO.Owner, 7, 1, IDs (S)), Writing, Number), "exact wide request" & Position'Image);
      end Request_Is;
      procedure Read_Trace (First : Natural; Stride, Datums : Positive) is
      begin
         Check (IO.Calls = 2 * Datums, "wide read request count");
         for I in 0 .. Datums - 1 loop
            Request_Is (2 * I + 1, Index_Port, True, AML_Decode.Integer_Value (First + I * Stride));
            Request_Is (2 * I + 2, Byte_Data, False);
         end loop;
      end Read_Trace;
      procedure Write_Trace (Bytes : AML_Decode.Bytes) is
      begin
         Check (IO.Calls = 2 * Bytes'Length + 1, "wide write request count");
         for I in 0 .. Bytes'Length - 1 loop
            Request_Is (2 * I + 1, Index_Port, True, AML_Decode.Integer_Value (4 + I));
            Request_Is (2 * I + 2, Byte_Data, True, AML_Decode.Integer_Value (Bytes (Bytes'First + I)));
         end loop;
         Request_Is (IO.Calls, Byte_Data, False);
      end Write_Trace;
      procedure Exercise_Faults is
         Name : constant AML_Names.Segment := (if Writing then "WP80" else "R080");
         Initial : constant Register_Values := [Index_Port => 0, Byte_Data => 16#EF#];
         Plan : Request_Log;
         Count : Natural;
         procedure Successful is
         begin
            if Writing then Expect (Name, 16#9A#);
            else Expect (Name, Bytes => [1 .. 10 => 16#EF#]); end if;
         end Successful;
         function Values_After (Completed : Natural) return Register_Values is
            Values : Register_Values := Initial;
         begin
            for P in 1 .. Completed loop
               if Plan (P).For_Write then
                  Values ((if Plan (P).Destination.Register = IDs (Index_Port)
                    then Index_Port else Byte_Data)) := Plan (P).Value;
               end if;
            end loop;
            return Values;
         end Values_After;
      begin
         IO.Registers := Initial;
         Successful;
         Plan := IO.Requests; Count := IO.Calls;
         Check (Count = (if Writing then 27 else 20), "wide fault baseline extent");
         for Fault in Fault_Kind range Denied_Before_Effect .. Unknown_After_Effect loop
            for At_Call in 1 .. Count loop
               Check (not Pending (IO) and then not F.Uncertain (Service).Present,
                 "wide clean before injection");
               IO.Registers := Initial; IO.Fault := Fault; IO.Fault_At := At_Call;
               IO.Confirmation := A.No_Observation;
               declare
                  Unknown : constant Boolean := Fault in Unknown_Before_Effect .. Unknown_After_Effect;
                  Completed : constant Natural := At_Call -
                    (if Fault in Denied_Before_Effect | Unknown_Before_Effect then 1 else 0);
                  Expected : constant Register_Values := Values_After (Completed);
               begin
                  Expect (Name, Want => (if Unknown then AML_Execute.Field_Indeterminate
                    elsif Writing and then At_Call < Count then AML_Execute.Unsupported
                    else AML_Execute.Unsupported_Value));
                  Check (IO.Calls = At_Call and then
                    IO.Requests (1 .. At_Call) = Plan (1 .. At_Call), "wide exact failed prefix");
                  Check (IO.Registers = Expected, "wide exact completed effects");
                  if Unknown then
                     Check (F.Uncertain (Service) = IO.Receipt and then
                       IO.Receipt.Item = Plan (At_Call), "wide exact unknown receipt");
                     Expect (Name, Want => AML_Execute.Field_Indeterminate);
                     Check (IO.Calls = 0 and then IO.Registers = Expected,
                       "wide unknown blocks automatic replay");
                     declare
                        Saved : constant A.Observation := IO.Receipt;
                        Wrong : A.Observation := Saved;
                     begin
                        F.Reconcile (Service, IO, Saved, Status);
                        Check (Status /= C.Values.Available, "wide unconfirmed receipt rejected");
                        IO.Receipt := A.No_Observation; IO.Confirmation := Saved;
                        Wrong.Operation := Wrong.Operation + 1;
                        F.Reconcile (Service, IO, Wrong, Status);
                        Check (Status /= C.Values.Available, "wide wrong receipt rejected");
                        F.Reconcile (Service, IO, Saved, Status);
                        Check (Status = C.Values.Available and then IO.Calls = 0,
                          "wide exact reconciliation has no effects");
                     end;
                  else
                     Check (not F.Uncertain (Service).Present and then not Pending (IO),
                       "wide known failure has no pending receipt");
                  end if;
                  IO.Registers := Initial;
                  Successful;
                  Check (IO.Calls = Count and then IO.Requests (1 .. Count) = Plan (1 .. Count)
                    and then IO.Registers = Values_After (Count), "wide explicit later invocation recovers");
               end;
            end loop;
         end loop;
      end Exercise_Faults;

   begin
      AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
      C.Stage_Table (Service, 1, C.DSDT, Raw, Staged); Check (Staged = C.Staged, "exact table stage");
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 100_000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed,
        "width module import: " & Execution.Modules.Outcome'Image & "/" & Execution.Modules.Failure'Image);
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
      F.Clear (Items);
      for S in Slot loop
         declare
            Name : constant AML_Names.Segment := (if S = Index_Port then "IDX0" else "DAT0");
            Node : C.Namespace.Node_ID;
            Description : C.Values.Field_Description;
            Key : C.Values.Field_Key;
         begin
            C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
            Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " leaf");
            C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
            Check (Status = C.Values.Available, Name & " direct descriptor");
            F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
            Check (Status = C.Values.Available, Name & " named leaf binding");
         end;
      end loop;
      if Faults then Exercise_Faults; return; end if;
      Expect ("INIT");
      Check (C.Observe (Service).Value_Bytes = 0, "fresh pressure arena bytes" & C.Observe (Service).Value_Bytes'Image);
      Expect ("ALOC", Want => AML_Execute.Value_Limit);
      Check (IO.Calls = 1, "full storage calls" & IO.Calls'Image & " bytes" & C.Observe (Service).Value_Bytes'Image);
      Request_Is (1, Index_Port, True, 16#77#);
      Expect ("R080", Bytes => [1 .. 10 => 16#EF#]); Read_Trace (4, 1, 10); Expect ("IDX0", 13);
      Expect ("RU80", Bytes => [1 .. 10 => 16#FE#]); Read_Trace (4, 1, 11); Expect ("IDX0", 14);
      Expect ("RP73", Bytes => [1 .. 9 => 16#FE#, 10 => 0]); Read_Trace (4, 1, 10); Expect ("IDX0", 13);
      Expect ("RW80", Bytes => [16#EF#,0,16#EF#,0,16#EF#,0,16#EF#,0,16#EF#,0]);
      Read_Trace (4, 2, 5); Expect ("IDX0", 12);
      Expect ("RQ80", Bytes => [16#0E#,0,0,0,0,0,0,16#F0#,16#0E#,0]);
      Read_Trace (0, 8, 2); Expect ("IDX0", 8);
      Expect ("WF80", 9); Write_Trace ([0,1,2,3,4,5,6,7,8,9]);
      Expect ("INIT"); Expect ("WP80", 16#9A#);
      Check (IO.Calls = 27, "wide partial Preserve requests");
      Request_Is (1, Index_Port, True, 4); Request_Is (2, Byte_Data, False);
      Request_Is (3, Index_Port, True, 4); Request_Is (4, Byte_Data, True, 16#0F#);
      for I in 1 .. 9 loop
         Request_Is (2 * I + 3, Index_Port, True, AML_Decode.Integer_Value (4 + I));
         Request_Is (2 * I + 4, Byte_Data, True, AML_Decode.Integer_Value (I * 16#11#));
      end loop;
      Request_Is (23, Index_Port, True, 14); Request_Is (24, Byte_Data, False);
      Request_Is (25, Index_Port, True, 14); Request_Is (26, Byte_Data, True, 16#9A#);
      Request_Is (27, Byte_Data, False);
      Expect ("INIT"); Expect ("WO80", 9); Write_Trace ([0,1,2,3,4,5,6,7,8,9]);
      Expect ("INIT"); Expect ("WZ80", 9); Write_Trace ([0,1,2,3,4,5,6,7,8,9]);
      Expect ("INIT"); Expect ("WS80"); Write_Trace ([16#78#,16#56#,16#34#,16#12#,0,0,0,0,0,0]);
      Expect ("INIT"); Expect ("WB80"); Write_Trace ([16#12#,16#34#,16#56#,0,0,0,0,0,0,0]);
      Expect ("RMAX", Want => AML_Execute.Value_Limit); Check (IO.Calls = 0, "oversize result before effects");
      Expect ("INIT"); Expect ("R080", Bytes => [1 .. 10 => 16#EF#]); Read_Trace (4, 1, 10);

   end Exercise;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      raise Program_Error with "expected baseline/read32/read64/write32/write64";
   end if;
   declare Mode : constant String := Ada.Command_Line.Argument (1); begin
      if Mode = "baseline" then Exercise (1); Exercise (2);
      elsif Mode = "read32" then Exercise (1, Faults => True);
      elsif Mode = "read64" then Exercise (2, Faults => True);
      elsif Mode = "write32" then Exercise (1, Faults => True, Writing => True);
      elsif Mode = "write64" then Exercise (2, Faults => True, Writing => True);
      else raise Program_Error with "unknown wide regression selection";
      end if;
   end;
   Ada.Text_IO.Put_Line ("Index width checks" & Natural'Image (Checks));
end Index_Width_Tests;
