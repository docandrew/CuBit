pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Execution_Limits;
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
   use type AML_Field_Datums.Update_Rule;
   subtype Slot is Positive range 1 .. 20;
   type Offset_Array is array (Slot) of Natural;
   Offsets : constant Offset_Array := [0,4,5,6,7,8,9,10,11,12,13,14,4,6,8,10,12,14,0,8];
   type Register_IDs is array (Slot) of A.Register_ID;
   IDs : constant Register_IDs := [for S in Slot => 16#A000_0000_0000_0000# + A.Register_ID (S)];
   type Permissions is array (Slot) of Boolean;
   type Request_Log is array (Positive range 1 .. 128) of A.Request;
   type Region_Bytes is array (Natural range 0 .. 31) of AML_Decode.Byte;
   type Fault_Kind is (No_Fault, Denied_Before_Effect, Failed_After_Effect,
     Unknown_Before_Effect, Unknown_After_Effect);
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Memory : Region_Bytes := [others => 0];
      Calls : Natural := 0;
      Requests : Request_Log := [others => (others => <>)];
      Attempts : Natural := 0;
      Attempted : Request_Log := [others => (others => <>)];
      Readable, Writable : Permissions := [others => True];
      Revoke_After : Natural range 0 .. Request_Log'Length := 0;
      Revoke_Register : A.Register_ID := A.No_Register;
      Revoke_Write, Revoked : Boolean := False;
      Fault : Fault_Kind := No_Fault;
      Fault_At : Natural range 0 .. Request_Log'Length := 0;
      Receipt, Confirmation : A.Observation := A.No_Observation;
      Next_Operation : A.Operation_ID := 1;
   end record;
   function Pending (IO : Context) return Boolean is
     (IO.Owner = AML_Identity.No_Identity or else IO.Receipt.Present);
   function Width_Of (S : Slot) return T.Access_Width is
     (if S <= 12 then T.Byte_Access elsif S <= 18 then T.Word_Access else T.Qword_Access);
   procedure Locate (ID : A.Register_ID; S : out Slot; Found : out Boolean) is
   begin
      S := Slot'First; Found := False;
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
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 32),
        Byte_Offset => AML_Decode.Integer_Value (Offsets (S)), Width => Width_Of (S));
      Accepted := Found and then IO.Owner /= AML_Identity.No_Identity
        and then Target.Provider = IO.Owner and then Target.Endpoint = 7 and then Target.Epoch = 1;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
      S : Slot;
      Found : Boolean;
   begin
      Describe (IO, Target, Geometry, Accepted); Locate (Target.Register, S, Found);
      Accepted := Accepted and then Found and then (if For_Write then IO.Writable (S) else IO.Readable (S));
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      S : Slot;
      Found, Allowed : Boolean;
      Number : AML_Decode.Integer_Value := 0;
      Fault : Fault_Kind;
      procedure Retain_Unknown is
      begin
         IO.Receipt := (True, IO.Owner, Item, IO.Next_Operation);
         Reply := (A.Indeterminate, IO.Owner, 0, IO.Next_Operation);
         IO.Next_Operation := IO.Next_Operation + 1;
      end Retain_Unknown;
   begin
      Reply := (others => <>);
      IO.Attempts := IO.Attempts + 1; IO.Attempted (IO.Attempts) := Item;
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
         for I in 0 .. T.Octets (Width_Of (S)) - 1 loop
            IO.Memory (Offsets (S) + I) := AML_Decode.Byte
              (Interfaces.Shift_Right (Item.Value, I * 8) and 16#FF#);
         end loop;
      else
         for I in 0 .. T.Octets (Width_Of (S)) - 1 loop
            Number := Number or Interfaces.Shift_Left
              (AML_Decode.Integer_Value (IO.Memory (Offsets (S) + I)), I * 8);
         end loop;
      end if;
      if IO.Revoke_After /= 0 and then IO.Calls = IO.Revoke_After then
         declare Revoked_Slot : Slot; Exists : Boolean; begin
            Locate (IO.Revoke_Register, Revoked_Slot, Exists);
            if not Exists then raise Program_Error with "unknown revoked register"; end if;
            if IO.Revoke_Write then IO.Writable (Revoked_Slot) := False;
            else IO.Readable (Revoked_Slot) := False; end if;
            IO.Revoke_After := 0; IO.Revoked := True;
         end;
      end if;
      if Fault = Failed_After_Effect then
         Reply := (A.Failed_Completed, IO.Owner, 0, A.No_Operation); return;
      end if;
      if Fault = Unknown_After_Effect then Retain_Unknown; return; end if;
      Reply := (A.Completed, IO.Owner, Number, A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin
      Accepted := Expected.Present and then not Pending (IO)
        and then Expected = IO.Confirmation;
   end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize);
   One_Work_Unit : constant AML_Execution_Limits.Field_Work_Limit := 1;
   One_Request : constant AML_Execution_Limits.Field_Request_Limit := 1;
   package Low_Work is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize, Maximum_Work => One_Work_Unit);
   package Low_Requests is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize, Maximum_Requests => One_Request);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   function Initial_Memory return Region_Bytes is
      Memory : Region_Bytes := [others => 0];
   begin
      for I in 0 .. 10 loop Memory (4 + I) := AML_Decode.Byte (16#10# + 17 * I); end loop;
      return Memory;
   end Initial_Memory;
   procedure Exercise (Revision : AML_Decode.Byte; Fault : Fault_Kind := No_Fault;
     Fault_Name : AML_Names.Segment := "RQ80"; Budgets : Boolean := False;
     Revocation : Boolean := False) is
      Raw : constant AML_Decode.Bytes := (if Revision = 1 then Index_Width_Oracle.Table32 else Index_Width_Oracle.Table64);
      Service : aliased C.State (1, Raw'Length, Raw'Length, 1);
      IO : Context;
      Items : F.Binding_Set;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Status : C.Values.Access_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Issued : Boolean;
      Region : C.Node_Descriptor;
      procedure Expect (Name : AML_Names.Segment; Bytes : AML_Decode.Bytes := [1 .. 0 => 0];
        Want : AML_Execute.Execution_Status := AML_Execute.Object_Returned) is
         Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor;
         Result : C.Values.Result;
         Description : C.Values.Value_Description;
         Handle : C.Values.Value_Handle;
         Data : AML_Decode.Bytes (1 .. Bytes'Length);
         Copied : Natural;
      begin
         IO.Calls := 0; IO.Attempts := 0;
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " lookup");
         C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, Name & " descriptor");
         F.Evaluate_Named (Service, IO, Items, Item, 100_000, Result, Status);
         Check (Status = C.Values.Available and then Result.Status = Want,
           Name & " expected " & Want'Image & " got " & Result.Status'Image);
         if Want = AML_Execute.Object_Returned then
            Handle := Result.Handle;
            C.Describe_Result (Service, Handle, Description, Status);
            Check (Status = C.Values.Available and then Description.Kind = C.Values.Buffer_Description
              and then Description.Length = Bytes'Length, Name & " exact buffer length");
            C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
            Check (Status = C.Values.Available and then Copied = Data'Length, Name & " buffer copy");
            for I in Data'Range loop Check (Data (I) = Bytes (Bytes'First + I - 1), Name & " byte" & I'Image); end loop;
            Check (C.Retained_Results (Service) = 1, Name & " one result");
            C.Release_Result (Service, Handle, Status);
            Check (Status = C.Values.Available and then Handle = C.Values.No_Value, Name & " release");
         elsif Want = AML_Execute.Returned then Check (Result.Number = 0, Name & " zero"); end if;
         Check (C.Retained_Results (Service) = 0, Name & " no retained roots");
      end Expect;
      procedure Fresh is
      begin
         Check (not Pending (IO) and then not F.Uncertain (Service).Present, "clean before region reset");
         IO.Readable := [others => True]; IO.Writable := [others => True];
         IO.Fault := No_Fault;
         IO.Revoke_After := 0; IO.Revoked := False;
         IO.Memory := [others => 0];
         Expect ("INIT", Want => AML_Execute.Returned);
         Check (IO.Memory = Initial_Memory, "fresh exact modeled memory");
      end Fresh;
      procedure Request_Is (Position : Positive; S : Slot; Writing : Boolean;
        Number : AML_Decode.Integer_Value := 0) is
      begin
         Check (Position <= IO.Calls and then IO.Requests (Position) =
           A.Request'((IO.Owner, 7, 1, IDs (S)), Writing, Number), "exact region request" & Position'Image);
      end Request_Is;
      procedure Read_Case (Name : AML_Names.Segment; Bytes : AML_Decode.Bytes;
        Bank : Boolean; First, Last : Slot) is
         P : Positive := 1;
         Memory : Region_Bytes := Initial_Memory;
      begin
         Fresh; Expect (Name, Bytes);
         for S in First .. Last loop
            if Bank then Request_Is (P, 1, True, 3); P := P + 1; end if;
            Request_Is (P, S, False); P := P + 1;
         end loop;
         Check (IO.Calls = P - 1, Name & " exact read extent");
         if Bank then Memory (0) := 3; end if;
         Check (IO.Memory = Memory, Name & " read effects only selector");
      end Read_Case;
      procedure Write_Case (Name : AML_Names.Segment; Bytes, Source : AML_Decode.Bytes;
        Bank, Unaligned : Boolean; Rule : AML_Field_Datums.Update_Rule := AML_Field_Datums.Preserve;
        Access_Width : T.Access_Width := T.Byte_Access) is
         Memory : Region_Bytes := Initial_Memory;
         Start : constant Natural := 32 + (if Unaligned then 4 else 0);
         Access_Bits : constant Positive := 8 * T.Octets (Access_Width);
         First : constant Slot := (case Access_Width is when T.Byte_Access => 2,
           when T.Word_Access => 13, when T.Qword_Access => 19, when others => raise Program_Error);
         Last : constant Slot := (case Access_Width is when T.Byte_Access => (if Unaligned then 12 else 11),
           when T.Word_Access => 17, when T.Qword_Access => 20, when others => raise Program_Error);
         P : Positive := 1;
         procedure Select_Bank is
         begin
            if Bank then Request_Is (P, 1, True, 3); P := P + 1; end if;
         end Select_Bank;
      begin
         for I in Start / Access_Bits * Access_Bits .. (Start + 79) / Access_Bits * Access_Bits + Access_Bits - 1 loop
            declare
               B : constant Natural := I / 8;
               Mask : constant AML_Decode.Byte := AML_Decode.Byte (2 ** (I mod 8));
               Bit_Value : AML_Decode.Byte;
            begin
               if I in Start .. Start + 79 then
                  Bit_Value := (if (I - Start) / 8 < Source'Length then
                    (Source (Source'First + (I - Start) / 8) / AML_Decode.Byte (2 ** ((I - Start) mod 8))) mod 2 else 0);
               elsif Rule = AML_Field_Datums.Preserve then
                  Bit_Value := (Memory (B) / Mask) mod 2;
               else Bit_Value := (if Rule = AML_Field_Datums.Write_As_Ones then 1 else 0);
               end if;
               Memory (B) := (Memory (B) and not Mask) or (Bit_Value * Mask);
            end;
         end loop;
         if Bank then Memory (0) := 3; end if;
         Fresh; Expect (Name, Bytes);
         for S in First .. Last loop
            declare Number : AML_Decode.Integer_Value := 0; begin
               if (Offsets (S) * 8 < Start or else (Offsets (S) + T.Octets (Access_Width)) * 8 > Start + 80)
                 and then Rule = AML_Field_Datums.Preserve then
                  Select_Bank; Request_Is (P, S, False); P := P + 1;
               end if;
               for I in 0 .. T.Octets (Access_Width) - 1 loop
                  Number := Number or Interfaces.Shift_Left (AML_Decode.Integer_Value (Memory (Offsets (S) + I)), I * 8);
               end loop;
               Select_Bank; Request_Is (P, S, True, Number); P := P + 1;
            end;
         end loop;
         for S in Slot range 2 .. 11 loop Request_Is (P, S, False); P := P + 1; end loop;
         Check (IO.Calls = P - 1, Name & " exact store extent");
         Check (IO.Memory = Memory, Name & " full exact store memory incl Preserve edges");
      end Write_Case;
      procedure Exercise_Faults is
         Plan : Request_Log;
         Count : Natural;
         Store_Requests : constant Natural :=
           (if Fault_Name = "SQ80" then 4 elsif Fault_Name = "SBU0" then 26 else 0);
         procedure Successful is
         begin
            if Fault_Name = "RQ80" then Expect (Fault_Name, Index_Width_Oracle.RQ80);
            elsif Fault_Name = "RB80" then Expect (Fault_Name, Index_Width_Oracle.RB80);
            elsif Fault_Name = "SQ80" then Expect (Fault_Name, Index_Width_Oracle.SQ80);
            elsif Fault_Name = "SBU0" then Expect (Fault_Name, Index_Width_Oracle.SBU0);
            else raise Program_Error with "unknown fault method";
            end if;
         end Successful;
         function Values_After (Completed : Natural) return Region_Bytes is
            Memory : Region_Bytes := Initial_Memory;
            S : Slot;
            Found : Boolean;
         begin
            for P in 1 .. Completed loop
               if Plan (P).For_Write then
                  Locate (Plan (P).Destination.Register, S, Found);
                  if not Found then raise Program_Error with "unknown plan target"; end if;
                  for I in 0 .. T.Octets (Width_Of (S)) - 1 loop
                     Memory (Offsets (S) + I) := AML_Decode.Byte
                       (Interfaces.Shift_Right (Plan (P).Value, I * 8) and 16#FF#);
                  end loop;
               end if;
            end loop;
            return Memory;
         end Values_After;
      begin
         Fresh; Successful; Plan := IO.Requests; Count := IO.Calls;
         Check (Count = (if Fault_Name = "RQ80" then 2 elsif Fault_Name = "RB80" then 20
           elsif Fault_Name = "SQ80" then 14 else 36), "fault plan extent");
         for At_Call in 1 .. Count loop
            Fresh; IO.Fault := Fault; IO.Fault_At := At_Call; IO.Confirmation := A.No_Observation;
            declare
               Unknown : constant Boolean := Fault in Unknown_Before_Effect .. Unknown_After_Effect;
               Completed : constant Natural := At_Call -
                 (if Fault in Denied_Before_Effect | Unknown_Before_Effect then 1 else 0);
               Expected : constant Region_Bytes := Values_After (Completed);
            begin
               Expect (Fault_Name, Want => (if Unknown then AML_Execute.Field_Indeterminate
                 elsif At_Call <= Store_Requests then AML_Execute.Unsupported
                 else AML_Execute.Unsupported_Value));
               Check (IO.Calls = At_Call and then IO.Requests (1 .. At_Call) = Plan (1 .. At_Call),
                 "region exact failed request prefix");
               Check (IO.Memory = Expected, "region exact completed effects");
               if Unknown then
                  Check (F.Uncertain (Service) = IO.Receipt and then IO.Receipt.Item = Plan (At_Call),
                    "region exact unknown receipt");
                  Expect (Fault_Name, Want => AML_Execute.Field_Indeterminate);
                  Check (IO.Calls = 0 and then IO.Memory = Expected, "region unknown blocks replay");
                  declare
                     Saved : constant A.Observation := IO.Receipt;
                     Wrong : A.Observation := Saved;
                  begin
                     F.Reconcile (Service, IO, Saved, Status);
                     Check (Status /= C.Values.Available, "region unconfirmed receipt rejected");
                     IO.Receipt := A.No_Observation; IO.Confirmation := Saved;
                     Wrong.Operation := Wrong.Operation + 1;
                     F.Reconcile (Service, IO, Wrong, Status);
                     Check (Status /= C.Values.Available, "region wrong operation rejected");
                     Wrong := Saved; Wrong.Item.Value := Wrong.Item.Value xor 1;
                     F.Reconcile (Service, IO, Wrong, Status);
                     Check (Status /= C.Values.Available, "region wrong request rejected");
                     F.Reconcile (Service, IO, Saved, Status);
                     Check (Status = C.Values.Available and then IO.Calls = 0 and then IO.Memory = Expected,
                       "region exact reconciliation has no I/O");
                  end;
               else
                  Check (not Pending (IO) and then not F.Uncertain (Service).Present,
                    "region known failure has no unknown receipt");
               end if;
               IO.Memory := Initial_Memory; Successful;
               Check (IO.Calls = Count and then IO.Requests (1 .. Count) = Plan (1 .. Count)
                 and then IO.Memory = Values_After (Count), "region explicit later invocation recovers");
            end;
         end loop;
      end Exercise_Faults;
      procedure Exercise_Revocation is
         Plan : Request_Log;
         Count : Natural;
         Store_Requests : constant Natural := (if Fault_Name = "SQ80" then 4
           elsif Fault_Name = "SBU0" then 26 elsif Fault_Name = "INIT" then 12 else 0);
         procedure Successful is
         begin
            if Fault_Name = "RQ80" then Expect (Fault_Name, Index_Width_Oracle.RQ80);
            elsif Fault_Name = "RB80" then Expect (Fault_Name, Index_Width_Oracle.RB80);
            elsif Fault_Name = "SQ80" then Expect (Fault_Name, Index_Width_Oracle.SQ80);
            elsif Fault_Name = "SBU0" then Expect (Fault_Name, Index_Width_Oracle.SBU0);
            elsif Fault_Name = "INIT" then Expect (Fault_Name, Want => AML_Execute.Returned);
            else raise Program_Error with "unknown revocation method"; end if;
         end Successful;
         function Values_After (Completed : Natural) return Region_Bytes is
            Memory : Region_Bytes := (if Fault_Name = "INIT" then [others => 16#A5#] else Initial_Memory);
            S : Slot;
            Found : Boolean;
         begin
            for P in 1 .. Completed loop
               if Plan (P).For_Write then
                  Locate (Plan (P).Destination.Register, S, Found);
                  if not Found then raise Program_Error with "unknown revocation plan target"; end if;
                  for I in 0 .. T.Octets (Width_Of (S)) - 1 loop
                     Memory (Offsets (S) + I) := AML_Decode.Byte
                       (Interfaces.Shift_Right (Plan (P).Value, I * 8) and 16#FF#);
                  end loop;
               end if;
            end loop;
            return Memory;
         end Values_After;
      begin
         if Fault_Name = "SEL0" then
            Fresh; IO.Readable (1) := False;
            Expect ("SEL0", Want => AML_Execute.Unsupported_Value);
            Check (IO.Calls = 0 and then IO.Attempts = 0 and then IO.Memory = Initial_Memory,
              "revoked scalar read rejected before provider dispatch");
            Fresh; IO.Writable (1) := False;
            Expect ("INIT", Want => AML_Execute.Unsupported);
            Check (IO.Calls = 0 and then IO.Attempts = 0 and then IO.Memory = Initial_Memory,
              "revoked scalar write rejected before provider dispatch");
            Fresh; Expect ("SEL0", Want => AML_Execute.Returned);
            Check (IO.Calls = 1 and then IO.Attempts = 1, "scalar read after explicit restoration");
            return;
         end if;
         Fresh; Successful; Plan := IO.Requests; Count := IO.Calls;
         Check (IO.Attempts = Count and then Count = (if Fault_Name = "RQ80" then 2
           elsif Fault_Name = "RB80" then 20 elsif Fault_Name = "SQ80" then 14
           elsif Fault_Name = "SBU0" then 36 else 12), "revocation baseline extent");
         for Completed in 1 .. Count - 1 loop
            Fresh;
            if Fault_Name = "INIT" then IO.Memory := [others => 16#A5#]; end if;
            IO.Revoke_After := Completed;
            IO.Revoke_Register := Plan (Completed + 1).Destination.Register;
            IO.Revoke_Write := Plan (Completed + 1).For_Write;
            Expect (Fault_Name, Want => (if Completed < Store_Requests then AML_Execute.Unsupported
              else AML_Execute.Unsupported_Value));
            Check (IO.Revoked and then IO.Calls = Completed and then IO.Attempts = Completed,
              "revocation stops before the next provider dispatch");
            Check (IO.Attempted (1 .. Completed) = Plan (1 .. Completed)
              and then IO.Requests (1 .. Completed) = Plan (1 .. Completed), "revocation exact admitted prefix");
            Check (IO.Memory = Values_After (Completed), "revocation exact completed effects");
            Check (not Pending (IO) and then not F.Uncertain (Service).Present,
              "revocation is known denial, not unknown completion");
            Fresh;
            if Fault_Name = "INIT" then IO.Memory := [others => 16#A5#]; end if;
            Successful;
            Check (IO.Calls = Count and then IO.Attempts = Count
              and then IO.Requests (1 .. Count) = Plan (1 .. Count)
              and then IO.Memory = Values_After (Count), "explicit restoration and new invocation recover");
         end loop;
      end Exercise_Revocation;
      procedure Exercise_Budgets is
         Work_Items : Low_Work.Binding_Set;
         Request_Items : Low_Requests.Binding_Set;
         Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Result : C.Values.Result;
      begin
         Low_Work.Clear (Work_Items); Low_Requests.Clear (Request_Items);
         C.Find_Child (Service, C.Namespace.Root, "SEL0", Node, Status);
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
         Check (Status = C.Values.Available, "budget selector descriptor");
         Low_Work.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (1)), Work_Items, Status);
         Check (Status = C.Values.Available, "budget work selector binding");
         Low_Requests.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (1)), Request_Items, Status);
         Check (Status = C.Values.Available, "budget request selector binding");
         for S in Slot loop
            Low_Work.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Work_Items, Status);
            Check (Status = C.Values.Available, "budget work region binding");
            Low_Requests.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Request_Items, Status);
            Check (Status = C.Values.Available, "budget request region binding");
         end loop;
         for Bank in Boolean loop
            declare
               Name : constant AML_Names.Segment := (if Bank then "RB80" else "RQ80");
               Memory : Region_Bytes := Initial_Memory;
            begin
               C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
               C.Describe_Node (Service, Node, Item, Status);
               Check (Status = C.Values.Available, "budget method descriptor");
               Fresh; IO.Calls := 0;
               Low_Work.Evaluate_Named (Service, IO, Work_Items, Item, 100_000, Result, Status);
               Check (Status = C.Values.Available and then Result.Status = AML_Execute.Value_Limit
                 and then IO.Calls = 0 and then IO.Memory = Memory, "wide work exhaustion before effects");
               Check (C.Retained_Results (Service) = 0 and then not Low_Work.Uncertain (Service).Present,
                 "work exhaustion no result/unknown");
               Fresh; IO.Calls := 0;
               Low_Requests.Evaluate_Named (Service, IO, Request_Items, Item, 100_000, Result, Status);
               if Bank then Memory (0) := 3; end if;
               Check (Status = C.Values.Available and then Result.Status = AML_Execute.Value_Limit
                 and then IO.Calls = 1 and then IO.Memory = Memory, "wide request exhaustion exact prefix");
               Request_Is (1, (if Bank then 1 else 19), Bank, (if Bank then 3 else 0));
               Check (C.Retained_Results (Service) = 0 and then not Low_Requests.Uncertain (Service).Present,
                 "request exhaustion no partial result/unknown");
               if Bank then Expect (Name, Index_Width_Oracle.RB80);
               else Expect (Name, Index_Width_Oracle.RQ80); end if;
            end;
         end loop;
      end Exercise_Budgets;
      Input : constant AML_Decode.Bytes := [0,1,2,3,4,5,6,7,8,9];
   begin
      AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
      C.Stage_Table (Service, 1, C.DSDT, Raw, Staged); Check (Staged = C.Staged, "table stage");
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 100_000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "module import");
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
      F.Clear (Items);
      declare N : C.Namespace.Node_ID; begin
         C.Find_Child (Service, C.Namespace.Root, "REG0", N, Status);
         C.Describe_Node (Service, N, Region, Status); Check (Status = C.Values.Available, "region descriptor");
      end;
      for S in Slot loop
         if S <= 12 then
            declare
               Name : AML_Names.Segment := "SEL0";
               Node : C.Namespace.Node_ID;
               Description : C.Values.Field_Description;
               Key : C.Values.Field_Key;
            begin
               if S > 1 then
                  Name := "D0" & Character'Val (Character'Pos ('0') + (S - 2) / 10)
                    & Character'Val (Character'Pos ('0') + (S - 2) mod 10);
               end if;
               C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
               C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
               Check (Status = C.Values.Available, Name & " leaf descriptor");
               F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
               Check (Status = C.Values.Available, Name & " leaf binding");
            end;
         end if;
         F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Items, Status);
         Check (Status = C.Values.Available, "region named register");
      end loop;
      if Budgets then Exercise_Budgets; return; end if;
      if Revocation then Exercise_Revocation; return; end if;
      if Fault /= No_Fault then Exercise_Faults; return; end if;
      Read_Case ("RD80", Index_Width_Oracle.RD80, False, 2, 11);
      Read_Case ("RU80", Index_Width_Oracle.RU80, False, 2, 12);
      Read_Case ("RP73", Index_Width_Oracle.RP73, False, 2, 11);
      Read_Case ("RW80", Index_Width_Oracle.RW80, False, 13, 17);
      Read_Case ("RQ80", Index_Width_Oracle.RQ80, False, 19, 20);
      Read_Case ("RB80", Index_Width_Oracle.RB80, True, 2, 11);
      Read_Case ("RBU0", Index_Width_Oracle.RBU0, True, 2, 12);
      Read_Case ("RBP3", Index_Width_Oracle.RBP3, True, 2, 11);
      Read_Case ("RBW0", Index_Width_Oracle.RBW0, True, 13, 17);
      Read_Case ("RBQ0", Index_Width_Oracle.RBQ0, True, 19, 20);
      Write_Case ("SD80", Index_Width_Oracle.SD80, Input, False, False);
      Write_Case ("SB80", Index_Width_Oracle.SB80, Input, True, False);
      Write_Case ("SDU0", Index_Width_Oracle.SDU0, Input, False, True);
      Write_Case ("SBU0", Index_Width_Oracle.SBU0, Input, True, True);
      Write_Case ("SDO0", Index_Width_Oracle.SDO0, Input, False, True, AML_Field_Datums.Write_As_Ones);
      Write_Case ("SBO0", Index_Width_Oracle.SBO0, Input, True, True, AML_Field_Datums.Write_As_Ones);
      Write_Case ("SDZ0", Index_Width_Oracle.SDZ0, Input, False, True, AML_Field_Datums.Write_As_Zeros);
      Write_Case ("SBZ0", Index_Width_Oracle.SBZ0, Input, True, True, AML_Field_Datums.Write_As_Zeros);
      Write_Case ("SDIN", Index_Width_Oracle.SDIN, [16#78#,16#56#,16#34#,16#12#], False, False);
      Write_Case ("SBIN", Index_Width_Oracle.SBIN, [16#78#,16#56#,16#34#,16#12#], True, False);
      Write_Case ("SDSH", Index_Width_Oracle.SDSH, [16#12#,16#34#,16#56#], False, False);
      Write_Case ("SBSH", Index_Width_Oracle.SBSH, [16#12#,16#34#,16#56#], True, False);
      Write_Case ("SW80", Index_Width_Oracle.SW80, Input, False, False, Access_Width => T.Word_Access);
      Write_Case ("SQ80", Index_Width_Oracle.SQ80, Input, False, False, Access_Width => T.Qword_Access);
      Write_Case ("SBW0", Index_Width_Oracle.SBW0, Input, True, False, Access_Width => T.Word_Access);
      Write_Case ("SBQ0", Index_Width_Oracle.SBQ0, Input, True, False, Access_Width => T.Qword_Access);
      Fresh; IO.Readable (12) := False;
      Expect ("RU80", Want => AML_Execute.Unsupported_Value);
      Check (IO.Calls = 0 and then IO.Memory = Initial_Memory, "direct missing last read permission before effects");
      Fresh; IO.Readable (11) := False;
      Expect ("RB80", Want => AML_Execute.Unsupported_Value);
      Check (IO.Calls = 0 and then IO.Memory = Initial_Memory, "bank missing last read permission before selection");
      Fresh; IO.Readable (2) := False;
      Expect ("SBU0", Want => AML_Execute.Unsupported);
      Check (IO.Calls = 0 and then IO.Memory = Initial_Memory, "bank Preserve read denial before effects");
      Fresh; IO.Writable (12) := False;
      Expect ("SDU0", Want => AML_Execute.Unsupported);
      Check (IO.Calls = 0 and then IO.Memory = Initial_Memory, "direct tail write denial before effects");
   end Exercise;
begin
   if Ada.Command_Line.Argument_Count /= 1 then raise Program_Error with "expected32/64"; end if;
   declare Mode : constant String := Ada.Command_Line.Argument (1); begin
      if Mode = "32" then Exercise (1);
      elsif Mode = "64" then Exercise (2);
      elsif Mode = "budget32" then Exercise (1, Budgets => True);
      elsif Mode = "budget64" then Exercise (2, Budgets => True);
      elsif Mode'Length = 8 and then Mode (1) = 'R' and then Mode (4) = '-' then
         if Mode (2 .. 3) not in "32" | "64" then raise Program_Error with "unknown revocation revision"; end if;
         Exercise ((if Mode (2 .. 3) = "32" then 1 else 2), Fault_Name => Mode (5 .. 8), Revocation => True);
      elsif Mode'Length > 8 and then Mode (3) = '-' and then Mode (8) = '-' then
         if Mode (1 .. 2) not in "32" | "64" then raise Program_Error with "unknown fault revision"; end if;
         Exercise ((if Mode (1 .. 2) = "32" then 1 else 2),
           Fault => Fault_Kind'Value (Mode (9 .. Mode'Last)), Fault_Name => Mode (4 .. 7));
      else raise Program_Error with "unknown revision"; end if;
   end;
   Ada.Text_IO.Put_Line ("Index width checks" & Natural'Image (Checks));
end Index_Width_Tests;
