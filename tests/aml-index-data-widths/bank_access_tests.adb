pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Bank_Access_Oracle;
procedure Bank_Access_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 96, Retained_Result_Capacity => 16);
   package A renames AML_Field_Access;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Operation_ID;
   use type A.Observation;
   use type A.Request;
   type Slot is (Selector, Data);
   type Registers is array (Slot) of AML_Decode.Integer_Value;
   type Register_IDs is array (Slot) of A.Register_ID;
   IDs : constant Register_IDs := [16#8000_0000_0000_0011#, 16#9000_0000_0000_0022#];
   Trace_Capacity : constant Positive := 256;
   subtype Request_Count is Natural range 0 .. Trace_Capacity;
   subtype Request_Position is Request_Count range 1 .. Request_Count'Last;
   type Request_Log is array (Request_Position range <>) of A.Request;
   type Fault_Kind is (No_Fault, Unknown_Before_Effect, Unknown_After_Effect);
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Values : Registers := [others => 0];
      Calls : Request_Count := 0;
      Trace : Request_Log (Request_Position) := [others => (others => <>)];
      Allow_Selector_Write, Allow_Data_Read, Allow_Data_Write : Boolean := True;
      Revoke_After_First : Boolean := False;
      Fault : Fault_Kind := No_Fault;
      Fault_At : Request_Count := 0;
      Receipt, Confirmed : A.Observation := A.No_Observation;
      Next_Operation : A.Operation_ID := 1;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 16),
        Byte_Offset => (if Target.Register = IDs (Selector) then 0 else 4),
        Width => (if Target.Register = IDs (Selector) then ACPI_FADT.Transactions.Byte_Access else ACPI_FADT.Transactions.Dword_Access));
      Accepted := IO.Owner /= AML_Identity.No_Identity and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then (Target.Register = IDs (Selector) or else Target.Register = IDs (Data));
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then
        (if Target.Register = IDs (Selector) then not For_Write or else IO.Allow_Selector_Write
         else (if For_Write then IO.Allow_Data_Write else IO.Allow_Data_Read)
           and then not (IO.Revoke_After_First and then IO.Calls > 0));
   end Authorize;
   procedure Retain_Unknown (IO : in out Context; Item : A.Request; Reply : out A.Response) is
   begin
      IO.Receipt := (True, IO.Owner, Item, IO.Next_Operation);
      Reply := (A.Indeterminate, IO.Owner, 0, IO.Next_Operation);
      IO.Next_Operation := IO.Next_Operation + 1;
   end Retain_Unknown;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Allowed : Boolean;
      S : constant Slot := (if Item.Destination.Register = IDs (Selector) then Selector else Data);
   begin
      Reply := (others => <>);
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Allowed or else (Item.For_Write and then Item.Value > (if S = Selector then 255 else 16#FFFF_FFFF#)) then return; end if;
      IO.Calls := IO.Calls + 1;
      IO.Trace (IO.Calls) := Item;
      if IO.Calls = IO.Fault_At and then IO.Fault = Unknown_Before_Effect then
         IO.Fault := No_Fault; Retain_Unknown (IO, Item, Reply); return;
      end if;
      if Item.For_Write then IO.Values (S) := Item.Value; end if;
      if IO.Calls = IO.Fault_At and then IO.Fault = Unknown_After_Effect then
         IO.Fault := No_Fault; Retain_Unknown (IO, Item, Reply); return;
      end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Values (S)), A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then Expected = IO.Confirmed; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Maximum_Bindings => 2, Authorize_Target => Authorize);
   Service : aliased C.State (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length);
   IO : Context;
   Items : F.Binding_Set;
   Stage : C.Stage_Status;
   Freeze : C.Freeze_Status;
   Status : C.Values.Access_Status;
   Modules : C.Execution_Report;
   Init : C.Namespace.Initialization_Report;
   Issued : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Expect (Name : AML_Names.Segment; Expected : AML_Decode.Integer_Value;
     Want : AML_Execute.Execution_Status := AML_Execute.Returned) is
      Node : C.Namespace.Node_ID;
      Descriptor : C.Node_Descriptor;
      Result : C.Values.Result;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status); Check (Status = C.Values.Available, "width lookup");
      C.Describe_Node (Service, Node, Descriptor, Status); Check (Status = C.Values.Available, "width descriptor");
      F.Evaluate_Named (Service, IO, Items, Descriptor, 100_000, Result, Status);
      Check (Status = C.Values.Available and then Result.Status = Want,
        Name & " expected status " & Want'Image & " got " & Result.Status'Image);
      if Want = AML_Execute.Returned then
         Check (Result.Number = Expected, Name & " expected" & Expected'Image & " got" & Result.Number'Image);
      end if;
      Check (C.Retained_Results (Service) = 0, "width roots");
   end Expect;
   type Operation_Kind is (Read_Byte, Read_Word, Read_Any, Read_Unaligned_Byte,
     Read_Unaligned_Twelve, Read_Sixteen, Read_Sixty_Four, Read_Aligned_Sixteen,
     Read_Aligned_Sixty_Four, Read_Qword, Write_Byte, Write_Word, Write_Qword,
     Write_Preserve, Write_Ones, Write_Zeros, Write_Ones_Byte);
   Initial_Data : constant AML_Decode.Integer_Value := 16#ABCD_1234#;
   function Name_Of (Op : Operation_Kind) return AML_Names.Segment is
     (case Op is when Read_Byte => "RB08", when Read_Word => "RB16", when Read_Any => "RANY",
       when Read_Unaligned_Byte => "RU08", when Read_Unaligned_Twelve => "RU12",
       when Read_Sixteen => "RU16", when Read_Sixty_Four => "RU64",
       when Read_Aligned_Sixteen => "RA16", when Read_Aligned_Sixty_Four => "RA64",
       when Read_Qword => "RQ64", when Write_Byte => "WB08", when Write_Word => "WB16",
       when Write_Qword => "WQ64", when Write_Preserve => "PB04", when Write_Ones => "WONE",
       when Write_Zeros => "WZER", when Write_Ones_Byte => "WON8");
   function Is_Write (Op : Operation_Kind) return Boolean is (Op >= Write_Byte);
   function Result_Of (Op : Operation_Kind) return AML_Decode.Integer_Value is
     (case Op is when Read_Byte | Read_Word | Read_Any | Read_Qword => Initial_Data,
       when Read_Unaligned_Byte => 16#63#, when Read_Unaligned_Twelve => 16#363#,
       when Read_Sixteen | Read_Aligned_Sixteen => 16#3434#,
       when Read_Sixty_Four | Read_Aligned_Sixty_Four => 16#3434_3434_3434_3434#,
       when others => 0);
   function Stored_Of (Op : Operation_Kind) return AML_Decode.Integer_Value is
     (case Op is when Write_Byte => 16#C3#, when Write_Word => 16#5678#,
       when Write_Qword => 16#5566_7788#, when Write_Preserve => 16#ABCD_1236#,
       when Write_Ones => 16#FFFF_FFF6#, when Write_Zeros => 6,
       when Write_Ones_Byte => 16#FFFF_FFA5#, when others => Initial_Data);
   type Request_Plan is record
      Count : Request_Count := 0;
      Items : Request_Log (Request_Position) := [others => (others => <>)];
   end record;
   function Plan_Of (Op : Operation_Kind) return Request_Plan is
      Result : Request_Plan;
      First : constant Positive := (case Op is
        when Read_Word | Read_Qword | Write_Word | Write_Qword => 8,
        when Read_Unaligned_Twelve => 5, when Read_Sixteen => 7,
        when Read_Sixty_Four => 9, when Read_Aligned_Sixty_Four => 6,
        when others => 4);
      Datums : constant Positive := (case Op is
        when Read_Unaligned_Byte | Read_Unaligned_Twelve | Read_Sixteen | Read_Aligned_Sixteen => 2,
        when Read_Sixty_Four | Read_Aligned_Sixty_Four => 8, when others => 1);
      procedure Add (S : Slot; Writing : Boolean; Number : AML_Decode.Integer_Value) is
      begin
         Result.Count := Result.Count + 1;
         Result.Items (Result.Count) := ((IO.Owner, 7, 1, IDs (S)), Writing, Number);
      end Add;
   begin
      if Op = Write_Preserve then
         Add (Selector, True, AML_Decode.Integer_Value (First)); Add (Data, False, 0);
      end if;
      for D in 0 .. Datums - 1 loop
         Add (Selector, True, AML_Decode.Integer_Value (First + D));
         Add (Data, Is_Write (Op), (if Is_Write (Op) then Stored_Of (Op) else 0));
      end loop;
      return Result;
   end Plan_Of;
   function Values_After (Plan : Request_Plan; Completed : Request_Count) return Registers is
      Result : Registers := [Selector => 0, Data => Initial_Data];
   begin
      for P in 1 .. Completed loop
         if Plan.Items (P).For_Write then
            Result ((if Plan.Items (P).Destination.Register = IDs (Selector) then Selector else Data)) := Plan.Items (P).Value;
         end if;
      end loop;
      return Result;
   end Values_After;
   procedure Reset_Model is
   begin
      Check (not F.Uncertain (Service).Present and then not Pending (IO), "clean before width reset");
      IO.Values := [Selector => 0, Data => Initial_Data]; IO.Calls := 0;
      IO.Allow_Selector_Write := True; IO.Allow_Data_Read := True; IO.Allow_Data_Write := True;
      IO.Revoke_After_First := False; IO.Fault := No_Fault; IO.Fault_At := 0;
      IO.Confirmed := A.No_Observation;
   end Reset_Model;
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "width provider identity");
   C.Stage_Table (Service, 1, C.DSDT, Bank_Access_Oracle.Raw_Table, Stage); Check (Stage = C.Staged, "width stage");
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "width freeze");
   C.Execute_Frozen (Service, 100_000, Modules, Status);
   Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed, "width module");
   C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "width members");
   F.Clear (Items);
   for S in Slot loop
      declare
         Node : C.Namespace.Node_ID;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Name : constant AML_Names.Segment := (if S = Selector then "IDX0" else "DAT0");
      begin
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status); Check (Status = C.Values.Available, "width key");
         F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
         Check (Status = C.Values.Available, "width explicit binding");
      end;
   end loop;
   Expect ("INIT", 16#0000000000000000#);
   Expect ("RB08", 16#00000000ABCD1234#);
   Expect ("IDX0", 16#0000000000000004#);
   Expect ("DAT0", 16#00000000ABCD1234#);
   Expect ("WB08", 16#0000000000000000#);
   Expect ("IDX0", 16#0000000000000004#);
   Expect ("DAT0", 16#00000000000000C3#);
   Expect ("INIT", 16#0000000000000000#);
   Expect ("PB04", 16#0000000000000000#);
   Expect ("IDX0", 16#0000000000000004#);
   Expect ("DAT0", 16#00000000ABCD1236#);
   Expect ("INIT", 16#0000000000000000#);
   Expect ("RB16", 16#00000000ABCD1234#);
   Expect ("IDX0", 16#0000000000000008#);
   Expect ("DAT0", 16#00000000ABCD1234#);
   Expect ("WB16", 16#0000000000000000#);
   Expect ("IDX0", 16#0000000000000008#);
   Expect ("DAT0", 16#0000000000005678#);
   Expect ("INIT", 16#0000000000000000#);
   Expect ("RANY", 16#00000000ABCD1234#);
   Expect ("IDX0", 16#0000000000000004#);
   Expect ("DAT0", 16#00000000ABCD1234#);
   Expect ("RU08", 16#0000000000000063#);
   Expect ("IDX0", 16#0000000000000005#);
   Expect ("RU12", 16#0000000000000363#);
   Expect ("IDX0", 16#0000000000000006#);
   Expect ("RU16", 16#0000000000003434#);
   Expect ("IDX0", 16#0000000000000008#);
   Expect ("RU64", 16#3434343434343434#);
   Expect ("IDX0", 16#0000000000000010#);
   Expect ("RA16", 16#0000000000003434#);
   Expect ("IDX0", 16#0000000000000005#);
   Expect ("RA64", 16#3434343434343434#);
   Expect ("IDX0", 16#000000000000000D#);
   Expect ("RQ64", 16#00000000ABCD1234#);
   Expect ("IDX0", 16#0000000000000008#);
   Expect ("WQ64", 16#0000000000000000#);
   Expect ("DAT0", 16#0000000055667788#);
   Expect ("INIT", 16#0000000000000000#);
   Expect ("WONE", 16#0000000000000000#);
   Expect ("DAT0", 16#00000000FFFFFFF6#);
   Expect ("INIT", 16#0000000000000000#);
   Expect ("WZER", 16#0000000000000000#);
   Expect ("DAT0", 16#0000000000000006#);
   Expect ("WON8", 16#0000000000000000#);
   Expect ("IDX0", 16#0000000000000004#);
   Expect ("DAT0", 16#00000000FFFFFFA5#);
   for Op in Operation_Kind loop
      declare
         Plan : constant Request_Plan := Plan_Of (Op);
         Name : constant AML_Names.Segment := Name_Of (Op);
         Final : constant Registers := Values_After (Plan, Plan.Count);
      begin
         Reset_Model; Expect (Name, Result_Of (Op));
         Check (IO.Calls = Plan.Count and then IO.Trace (1 .. IO.Calls) = Plan.Items (1 .. Plan.Count),
           Name & " exact width request order/values");
         Check (IO.Values = Final, Name & " complete width effects");
         Reset_Model; IO.Allow_Selector_Write := False;
         Expect (Name, 0, (if Is_Write (Op) then AML_Execute.Unsupported else AML_Execute.Unsupported_Value));
         Check (IO.Calls = 0 and then IO.Values = Values_After (Plan, 0), "selector denial before effects");
         Reset_Model; IO.Allow_Data_Read := False;
         if Is_Write (Op) and then Op /= Write_Preserve then
            Expect (Name, 0);
            Check (IO.Calls = Plan.Count and then IO.Values = Final, "whole/Ones/Zeros need no data read");
         else
            Expect (Name, 0, (if Is_Write (Op) then AML_Execute.Unsupported else AML_Execute.Unsupported_Value));
            Check (IO.Calls = 0 and then IO.Values = Values_After (Plan, 0), "read denial before effects");
         end if;
         if Is_Write (Op) then
            Reset_Model; IO.Allow_Data_Write := False;
            Expect (Name, 0, (if Is_Write (Op) then AML_Execute.Unsupported else AML_Execute.Unsupported_Value));
            Check (IO.Calls = 0 and then IO.Values = Values_After (Plan, 0), "write denial before effects");
         end if;
         Reset_Model; IO.Revoke_After_First := True;
         Expect (Name, 0, (if Is_Write (Op) then AML_Execute.Unsupported else AML_Execute.Unsupported_Value));
         Check (IO.Calls = 1 and then IO.Trace (1) = Plan.Items (1)
           and then IO.Values = Values_After (Plan, 1), "revocation retains completed selector only");
         for Fault in Fault_Kind range Unknown_Before_Effect .. Unknown_After_Effect loop
            for At_Call in 1 .. Plan.Count loop
               Reset_Model; IO.Fault := Fault; IO.Fault_At := At_Call;
               Expect (Name, 0, AML_Execute.Field_Indeterminate);
               Check (IO.Calls = At_Call and then IO.Trace (1 .. At_Call) = Plan.Items (1 .. At_Call),
                 "width uncertainty exact request prefix");
               Check (F.Uncertain (Service) = IO.Receipt and then IO.Receipt.Item = Plan.Items (At_Call),
                 "width uncertainty exact full target/direction/value");
               Check (IO.Values = Values_After (Plan, At_Call - (if Fault = Unknown_Before_Effect then 1 else 0)),
                 "width completed effects preserved");
               Expect (Name, 0, AML_Execute.Field_Indeterminate);
               Check (IO.Calls = At_Call, "width unknown never automatically replayed");
               declare Saved : constant A.Observation := IO.Receipt; Wrong : A.Observation := Saved; begin
                  F.Reconcile (Service, IO, Saved, Status);
                  Check (Status /= C.Values.Available and then IO.Calls = At_Call, "width unconfirmed reconcile denied");
                  IO.Receipt := A.No_Observation; IO.Confirmed := Saved;
                  Wrong.Operation := Wrong.Operation + 1;
                  F.Reconcile (Service, IO, Wrong, Status); Check (Status /= C.Values.Available, "wrong width operation denied");
                  Wrong := Saved; Wrong.Item.Value := Wrong.Item.Value xor 1;
                  F.Reconcile (Service, IO, Wrong, Status); Check (Status /= C.Values.Available, "wrong width request denied");
                  F.Reconcile (Service, IO, Saved, Status); Check (Status = C.Values.Available, "exact width reconcile");
               end;
               Expect (Name, Result_Of (Op));
               Check (IO.Calls = At_Call + Plan.Count
                 and then IO.Trace (At_Call + 1 .. IO.Calls) = Plan.Items (1 .. Plan.Count)
                 and then IO.Values = Final, "width explicit invocation after confirmed completion");
            end loop;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Bank access checks" & Checks'Image);
end Bank_Access_Tests;
