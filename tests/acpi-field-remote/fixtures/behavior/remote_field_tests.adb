pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with ACPI_Field_Remote_Provider;
with ACPI_Region_IO;
with ACPI_Region_Protocol;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Plans;
with AML_Field_Targets;
with AML_Field_Layout;
with Root_Test_Support; use Root_Test_Support;
with Qword_Oracle;
procedure Remote_Field_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package T renames ACPI_FADT.Transactions;
   package A renames AML_Field_Access;
   package E renames AML_Execute;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Kind;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   use type E.Execution_Status;
   use type T.Access_Width;
   use type ACPI_Region_Policy.Address_Space;
   use type Qword_Oracle.Memory;
   use type A.Completion;
   use type A.Operation_ID;
   type Memory is array (Natural range 0 .. 15) of Unsigned_8;
   Initial : constant Memory := [16#A5#,16#5A#,16#3C#,16#C3#,16#96#,16#69#,16#F0#,16#0F#,
      16#33#,16#CC#,16#87#,16#78#,16#58#,16#65#,16#72#,16#8F#];
   type Hardware is record
      Data : Memory := Initial;
      Reads, Writes : Natural := 0;
   end record;
   Unknown_Read, Unknown_Write, Revoke_On_Read : Integer range -1 .. 8 := -1;
   Writes_Revoked : Boolean := False;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
   procedure Transact (H : in out Hardware; Space : ACPI_Region_Policy.Address_Space;
      Address : Unsigned_64; Width : T.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      Offset : constant Natural := Natural (Address - 16#1000#);
   begin
      Check (Space = ACPI_Region_Policy.Memory_Space and then Width = T.Qword_Access
         and then Offset in 0 | 8, "explicit memory Qword transaction");
      Output := 0; Completed := True;
      if For_Write then
         H.Writes := H.Writes + 1;
         for I in 0 .. 7 loop H.Data (Offset + I) := Unsigned_8 (Shift_Right (Input, I * 8) and 255); end loop;
         if Offset = Unknown_Write then Unknown_Write := -1; Completed := False; end if;
      else
         H.Reads := H.Reads + 1;
         for I in 0 .. 7 loop Output := Output or Shift_Left (Unsigned_64 (H.Data (Offset + I)), I * 8); end loop;
         if Offset = Unknown_Read then Unknown_Read := -1; Completed := False; end if;
         if Offset = Revoke_On_Read then Writes_Revoked := True; end if;
      end if;
   end Transact;
   package Backend is new ACPI_Region_IO (Hardware, Transact);
   package Wire renames ACPI_Region_Protocol;
   type Peer is new Unsigned_64;
   type Transport is null record;
   type Slot is range 1 .. 2;
   type States is array (Slot) of ACPI_Region_Policy.State;
   Model : Hardware;
   Regions : States;
   Calls : Natural := 0;
   Pending_Slot : Natural := 0;
   Pending_Ticket : Unsigned_64 := 0;
   procedure Exchange
     (State : in out Transport; Destination : Peer; Deadline : Wire.Time_Instant;
      Item : Wire.Packet; Reply : out Wire.Packet; Delivered : out Wire.Delivery) is
      pragma Unreferenced (State);
      use type Wire.Time_Instant;
   begin
      if Destination /= 1 or else Deadline /= 99 then raise Program_Error with "transport binding"; end if;
      Calls := Calls + 1;
      Delivered := Wire.Reply_Received;
      Reply := (Label => Backend.Operation_Reply, Data => [0,0,0,0], others => <>);
      for S in Regions'Range loop
         if ACPI_Region_Policy.Policy (Regions (S)).Register_Authority.Resource_ID = Item.Data (1) then
            Backend.Dispatch (Regions (S), Model,
              ACPI_Region_Policy.Policy (Regions (S)).Tag, Item, Reply, Pending_Ticket);
            if Pending_Ticket /= 0 then Pending_Slot := Natural (S); end if;
            return;
         end if;
      end loop;
   end Exchange;
   procedure Window (Now, Deadline : out Wire.Time_Instant) is
   begin Now := 10; Deadline := 99; end Window;
   package P is new ACPI_Field_Remote_Provider
     (Transport, Peer, 0, Exchange, Window, Maximum_Registers => 2, Maximum_Confirmations => 2);
   function Observe (IO : P.Context) return Hardware is
      pragma Unreferenced (IO);
   begin return Model; end Observe;
   procedure Confirm_Model (IO : in out P.Context; Saved : A.Observation; Accepted : out Boolean) is
      Finished : Boolean;
   begin
      if Pending_Slot = 0 or else Pending_Ticket = 0 then raise Program_Error with "missing backend receipt"; end if;
      ACPI_Region_Policy.Finish_Access (Regions (Slot (Pending_Slot)), Pending_Ticket, Finished);
      if not Finished then raise Program_Error with "backend completion"; end if;
      Pending_Slot := 0; Pending_Ticket := 0;
      P.Confirm_Completion (IO, Saved, True, Accepted);
   end Confirm_Model;
   procedure Authorize (IO : P.Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin
      P.Authorize (IO, Target, For_Write, Accepted);
      Accepted := Accepted and then not (For_Write and then Writes_Revoked);
   end Authorize;
   package F is new C.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled, Authorize_Target => Authorize);
   type Source_Kind is (Zero_Value, Ones_Value, Pattern_Value, Wide_Value,
      Buffer_Value, String_Value, Empty_Buffer, Empty_String);
   type Rule is (Preserve, Write_Ones, Write_Zeros);
   function Table (Revision : Unsigned_8; First : Natural; Update : Rule) return Bytes is
      Flag : constant Unsigned_8 := 4 + Unsigned_8 (Rule'Pos (Update) * 32);
      Payload : constant Bytes := [16#5B#,16#80#] & Enc ("REG0") & [0,16#0C#,0,16#10#,0,0,16#0A#,16]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [Flag]
           & (if First = 0 then Bytes'[] else Bytes'[0,Unsigned_8 (First)]) & Enc ("FLD0") & [16#40#,4])
        & Method ("READ", [16#A4#] & Enc ("FLD0"))
        & Block ([16#14#], Enc ("WRIT") & [1,16#70#,16#68#] & Enc ("FLD0") & [16#A4#,0])
        & Method ("BUFR", [16#70#] & Block ([16#11#], [16#0A#,8,16#C7#,16#34#,16#12#,16#5A#,16#96#,16#69#,16#A5#,16#3C#]) & Enc ("FLD0") & [16#A4#,0])
        & Method ("STRG", [16#70#,16#0D#] & Enc ("Az2!") & [0] & Enc ("FLD0") & [16#A4#,0])
        & Method ("EBUF", [16#70#] & Block ([16#11#], [0]) & Enc ("FLD0") & [16#A4#,0])
        & Method ("ESTR", [16#70#,16#0D#,0] & Enc ("FLD0") & [16#A4#,0]);
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255); end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
   type Binding_Mode is (Planned_Bindings, Exact_Single, Exact_Set);
   procedure Exercise (Revision : Unsigned_8; First : Natural; Update : Rule;
      Source : Source_Kind; Policy : Natural := 0; Fault : Natural := 0;
      Mode : Binding_Mode := Planned_Bindings) is
      S : aliased C.State (1,4096,4096);
      IO : P.Context;
      Items, Read_Only : F.Binding_Set;
      Single : F.Binding;
      Targets : AML_Field_Targets.Vector := [others => A.No_Target];
      Count : constant AML_Field_Plans.Datum_Count := (if First = 0 then 1 else 2);
      IDs : constant array (Positive range 1 .. 2) of A.Register_ID :=
        [16#FFFF_FFFF_FFFF_FF97#,16#8000_0000_0000_0041#];
      Raw : constant Unsigned_64 := (case Source is
         when Zero_Value | Empty_Buffer | Empty_String => 0,
         when Ones_Value => Unsigned_64'Last, when Pattern_Value => 16#5A12_34C7#,
         when Wide_Value => 16#FEDC_BA98_7654_3210#,
         when Buffer_Value => 16#3CA5_6996_5A12_34C7#, when String_Value => 16#2132_7A41#);
      Input : constant Unsigned_64 := (if Revision = 1 and then Source <= Wide_Value
         then Raw and 16#FFFF_FFFF# else Raw);
      Name : constant String := (case Source is when Buffer_Value => "BUFR", when String_Value => "STRG",
         when Empty_Buffer => "EBUF", when Empty_String => "ESTR", when others => "WRIT");
      Expected : Memory := Initial;
      Config : ACPI_Region_Policy.Configuration;
      Status : C.Values.Access_Status;
      Key : C.Values.Field_Key;
      Description : C.Values.Field_Description;
      Found : C.Namespace.Lookup_Result;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Result : E.Execution_Result;
      Saved : A.Observation;
      Before, After : Hardware;
      OK, Bit_Value : Boolean;
      procedure Write_Value (Set : F.Binding_Set; Want : E.Execution_Status;
         Use_Exact : Boolean := False) is
      begin
         C.Find_Object (S,C.Namespace.Root,[Name (1),Name (2),Name (3),Name (4)],Found,Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "write method");
         if Use_Exact and then Mode = Exact_Single then
            F.Invoke_Scalar (S,IO,Single,Found.Node,[0 => Raw, others => 0],
               (if Name = "WRIT" then 1 else 0),1000,Result);
         else
            F.Invoke_Scalar (S,IO,Set,Found.Node,[0 => Raw, others => 0],
               (if Name = "WRIT" then 1 else 0),1000,Result);
         end if;
         Check (Result.Status = Want, "write outcome" & Result.Status'Image);
         Check (C.Retained_Results (S) = 0,"write root cleanup");
      end Write_Value;
      procedure Invoke_Read (Named : out C.Values.Result) is
      begin
         C.Find_Object (S,C.Namespace.Root,"READ",Found,Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found,"read method");
         case Mode is
            when Planned_Bindings =>
               F.Invoke_Retained (S,IO,Read_Only,Found.Node,[others => 0],0,1000,Named,Status);
            when Exact_Single =>
               F.Invoke_Retained (S,IO,Single,Found.Node,[others => 0],0,1000,Named,Status);
            when Exact_Set =>
               F.Invoke_Retained (S,IO,Items,Found.Node,[others => 0],0,1000,Named,Status);
         end case;
      end Invoke_Read;
      procedure Read_Value (Image : Memory) is
         Number : Unsigned_64 := 0;
         Named : C.Values.Result;
         View : C.Values.Value_Description;
         Data : Bytes (1 .. 8);
         Copied : Natural;
         Handle : C.Values.Value_Handle;
      begin
         for I in 0 .. 63 loop
            if (Shift_Right (Unsigned_64 (Image ((First + I) / 8)),(First + I) mod 8) and 1) /= 0 then
               Number := Number or Shift_Left (Unsigned_64'(1),I);
            end if;
         end loop;
         Invoke_Read (Named);
         Check (Status = C.Values.Available,"read access");
         if Revision = 1 then
            Check (Named.Status = E.Object_Returned,"32bit AML returns Buffer: " & Named.Status'Image
               & Mode'Image & Revision'Image & Update'Image & Source'Image & Fault'Image);
            C.Describe_Result (S,Named.Handle,View,Status);
            Check (Status = C.Values.Available and then View.Kind = C.Values.Buffer_Description and then View.Length = 8,"eight byte Buffer");
            C.Read_Result_Bytes (S,Named.Handle,0,Data,Copied,Status);
            Check (Status = C.Values.Available and then Copied = 8,"copy Buffer");
            for I in 0 .. 7 loop Check (Data (I + 1) = Unsigned_8 (Shift_Right (Number,I * 8) and 255),"read byte value"); end loop;
            Handle := Named.Handle; C.Release_Result (S,Handle,Status);
            Check (Status = C.Values.Available,"release Buffer");
         else
            Check (Named.Status = E.Returned and then Named.Number = Number,"64bit read value");
         end if;
         Check (C.Retained_Results (S) = 0,"read root cleanup");
      end Read_Value;
   begin
      Unknown_Read := -1; Unknown_Write := -1; Revoke_On_Read := -1; Writes_Revoked := False;
      Model := (others => <>); Calls := 0; Pending_Slot := 0; Pending_Ticket := 0;
      declare Fresh : States; begin Regions := Fresh; end;
      P.Initialize (IO,1,7,OK); Check (OK,"initialize remote provider");
      C.Stage_Table (S,1,C.DSDT,Table (Revision,First,Update),Staged); Check (Staged = C.Staged,"stage");
      C.Freeze_Catalog (S,Frozen); Check (Frozen = C.Catalog_Frozen_OK,"freeze");
      C.Execute_Frozen (S,10000,Report,Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed,"module");
      C.Initialize_Members (S,Members,Status); Check (Status = C.Values.Available,"members");
      C.Find_Object (S,C.Namespace.Root,"FLD0",Found,Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found,"field");
      C.Read_Field_Descriptor (S,Found.Node,Description,Key,Status); Check (Status = C.Values.Available,"descriptor");
      for Position in AML_Field_Plans.Datum_Position range 1 .. Count loop
         Config := (Tag => 17,Base => 16#1000#,Length => 16,Space => ACPI_Region_Policy.Memory_Space,
            Readable => Policy /= 1,Writable => Policy /= 2,Widths => [others => False],
            Register_Authority => (Resource_ID => Unsigned_64 (IDs (Positive (Position))),Readable => Policy /= 1,
               Writable => Policy /= 2,Delegable => False,Write_Mask => (if Policy = 2 then 0 elsif Policy = 3 then 1 else Unsigned_64'Last)),
            Register_Offset => Unsigned_64 ((Natural (Position) - 1) * 8),Register_Width => T.Qword_Access);
         Config.Widths (T.Qword_Access) := True;
         ACPI_Region_Policy.Install (Regions (Slot (Position)),Config,OK);
         if not OK then raise Program_Error with "backend installation"; end if;
         P.Install (IO,(Register => IDs (Positive (Position)), Epoch => 1,
           Geometry => (Region => (Space => 0, Base => Config.Base, Length => Config.Length),
             Byte_Offset => Config.Register_Offset, Width => Config.Register_Width),
           Readable => Config.Readable, Writable => Config.Writable,
           Exact_Write_Safe => Policy /= 4, Write_Mask => Config.Register_Authority.Write_Mask),OK);
         Check (OK,"named Qword install");
         Targets (Position) := (P.Identity (IO),7,1,IDs (Positive (Position)));
      end loop;
      F.Add_Read_Binding (S,IO,Key,Targets,Count,Read_Only,Status); Check (Status = C.Values.Available,"read association");
      Write_Value (Read_Only,E.Unsupported); Check (Observe (IO).Reads = 0 and then Observe (IO).Writes = 0,"read association never authorizes write");
      case Mode is
         when Planned_Bindings => F.Add_Write_Binding (S,IO,Key,Targets,Count,Items,Status);
         when Exact_Single => F.Bind (S,IO,Key,Targets (1),Single,Status);
         when Exact_Set => F.Add_Binding (S,IO,Key,Targets (1),Items,Status);
      end case;
      if Policy in 2 .. 4 or else (Policy = 1 and then Update = Preserve and then First /= 0) then
         Check (Status /= C.Values.Available,"permission preflight denial");
         Check (Observe (IO).Reads = 0 and then Observe (IO).Writes = 0,"denied no effects"); return;
      end if;
      Check (Status = C.Values.Available,"write association");
      if Fault = 8 then
         declare
            Named : C.Values.Result;
         begin
            Before := Observe (IO); Unknown_Read := 0;
            Invoke_Read (Named);
            Check (Named.Status = E.Field_Indeterminate and then C.Retained_Results (S) = 0,
               "exact unknown read releases staged result");
            After := Observe (IO);
            Check (After.Reads = Before.Reads + 1 and then After.Writes = Before.Writes,
               "one unknown read effect");
            Saved := F.Uncertain (S);
            Check (Saved.Present and then not Saved.Item.For_Write and then P.Pending (IO),
               "exact unknown read receipt");
            Invoke_Read (Named);
            Check (Named.Status = E.Field_Indeterminate and then Observe (IO) = After
               and then C.Retained_Results (S) = 0,"exact read no replay");
            Confirm_Model (IO,Saved,OK); Check (OK,"exact read trusted completion");
            F.Reconcile (S,IO,Saved,Status);
            Check (Status = C.Values.Available and then Observe (IO) = After,"exact read reconcile");
            Unknown_Read := -1;
            Invoke_Read (Named);
            Check (Named.Status = E.Unsupported_Value and then Observe (IO) = After
               and then C.Retained_Results (S) = 0 and then not P.Pending (IO)
               and then not ACPI_Region_Policy.Active (Regions (1)),
               "read reconciliation never restores revoked backend authority");
            P.Retire_Confirmation (IO,Saved,True,OK);
            Check (OK,"retire exact arena-consumed read receipt");
            ACPI_Region_Policy.Install (Regions (1),Config,OK);
            Check (OK and then ACPI_Region_Policy.Epoch (Regions (1)) = 2,
               "explicit trusted backend regrant advances epoch");
            P.Revoke_Register (IO,Targets (1),OK); Check (OK,"revoke old metadata before replacement");
            P.Install (IO,(Register => IDs (1),Epoch => 2,
              Geometry => (Region => (Space => 0,Base => Config.Base,Length => Config.Length),
                Byte_Offset => 0,Width => T.Qword_Access),Readable => True,Writable => True,
              Exact_Write_Safe => True,Write_Mask => Unsigned_64'Last),OK);
            Check (OK,"new epoch metadata association");
            Targets (1).Epoch := 2; F.Clear (Items);
            case Mode is
               when Planned_Bindings => F.Add_Write_Binding (S,IO,Key,Targets,Count,Items,Status);
               when Exact_Single => F.Bind (S,IO,Key,Targets (1),Single,Status);
               when Exact_Set => F.Add_Binding (S,IO,Key,Targets (1),Items,Status);
            end case;
            Check (Status = C.Values.Available,"explicit new epoch binding");
            Read_Value (Initial); return;
         end;
      end if;
      for Bit in 0 .. Natural (Count) * 64 - 1 loop
         Bit_Value := (if Bit in First .. First + 63 then (Shift_Right (Input,Bit - First) and 1) /= 0
            else (case Update is when Preserve => (Shift_Right (Unsigned_64 (Initial (Bit / 8)),Bit mod 8) and 1) /= 0,
               when Write_Ones => True, when Write_Zeros => False));
         if Bit_Value then Expected (Bit / 8) := Expected (Bit / 8) or Shift_Left (Unsigned_8'(1),Bit mod 8);
         else Expected (Bit / 8) := Expected (Bit / 8) and not Shift_Left (Unsigned_8'(1),Bit mod 8); end if;
      end loop;
      if Policy = 0 and then Fault = 0 then Read_Value (Initial); end if;
      Before := Observe (IO);
      case Fault is
         when 1 => Unknown_Read := 0;
         when 2 => Unknown_Write := 8;
         when 4 => P.Revoke_Register (IO,Targets (Count),OK); Check (OK,"revoke last target");
         when 5 => Revoke_On_Read := 0;
         when 6 => Revoke_On_Read := 8;
         when 7 => Unknown_Write := 0;
         when others => null;
      end case;
      Write_Value (Items,(if Fault in 1 .. 2 | 7 then E.Field_Indeterminate elsif Fault /= 0 then E.Unsupported else E.Returned),
         Use_Exact => Mode /= Planned_Bindings);
      After := Observe (IO);
      if Fault = 0 then
         Check (After.Data = Expected,"independent bit model");
         if Policy = 0 and then Source not in Wide_Value | Empty_Buffer | Empty_String then
            Check (Qword_Oracle.Memory (After.Data) = Qword_Oracle.Expected
              ((((Natural (Revision) - 1) * 3 + (if First = 0 then 0 elsif First = 7 then 1 else 2)) * 3
                + Rule'Pos (Update)) * 5 + (if Source <= Pattern_Value then Source_Kind'Pos (Source)
                  else Source_Kind'Pos (Source) - 1)),"normal ACPICA byte values");
         end if;
         Check (After.Writes = Before.Writes + Natural (Count) and then
            After.Reads = Before.Reads + (if Update = Preserve and then First /= 0 then 2 else 0),"update access count");
         if Policy = 0 then Read_Value (Expected); end if;
      elsif Fault = 4 then Check (After = Before,"final target preflight before prefix");
      elsif Fault in 1 | 5 then Check (After.Writes = Before.Writes and then After.Data = Initial,"stop before first write");
      elsif Fault = 2 then Check (After.Writes = 2 and then After.Data = Expected,"unknown second write retains effects");
      elsif Fault = 7 then Check (After.Writes = 1 and then After.Data = Expected,"exact unknown write retains effect");
      else
         Check (After.Writes = 1 and then After.Data (0 .. 7) = Expected (0 .. 7)
            and then After.Data (8 .. 15) = Initial (8 .. 15),"prefix retained on permission change");
      end if;
      if Fault in 1 .. 2 | 7 then
         Saved := F.Uncertain (S); Check (Saved.Present and then P.Pending (IO),"unknown receipt");
         Check (Saved.Item.For_Write = (Fault in 2 | 7),"unknown direction");
         Before := After; Write_Value (Items,E.Field_Indeterminate,Use_Exact => Mode /= Planned_Bindings);
         Check (Observe (IO) = Before,"no retry");
         Confirm_Model (IO,Saved,OK); Check (OK,"trusted completion");
         F.Reconcile (S,IO,Saved,Status); Check (Status = C.Values.Available and then Observe (IO) = Before,"reconcile without replay");
      end if;
   end Exercise;
   procedure Exercise_History is
      IO, Other : P.Context;
      IDs : constant array (Slot) of A.Register_ID := [16#FEDC_1111#,16#ABCD_2222_3333_4444#];
      Targets : array (Slot) of A.Target;
      D : P.Descriptor;
      Config : ACPI_Region_Policy.Configuration;
      Saved, Second, Wrong : A.Observation;
      Item : A.Request;
      Reply : A.Response;
      Geometry : AML_Field_Layout.Register_Geometry;
      OK : Boolean;
      Before : Natural;
   begin
      Model := (others => <>); Calls := 0; Pending_Slot := 0; Pending_Ticket := 0;
      Unknown_Read := -1; Unknown_Write := -1; Revoke_On_Read := -1; Writes_Revoked := False;
      declare Fresh : States; begin Regions := Fresh; end;
      P.Initialize (IO,0,7,OK); Check (not OK,"no transport endpoint");
      P.Initialize (IO,1,0,OK); Check (not OK,"no public symbol");
      P.Initialize (IO,1,7,OK); Check (OK,"history initialization");
      P.Initialize (IO,2,8,OK); Check (not OK,"no retarget");
      P.Initialize (Other,1,7,OK); Check (OK,"other context");
      D := (Register => IDs (1), Epoch => 1,
        Geometry => (Region => (Space => 0, Base => 16#1000#, Length => 16),
          Byte_Offset => 0, Width => T.Qword_Access),
        Readable => True, Writable => True, Exact_Write_Safe => True,
        Write_Mask => Unsigned_64'Last);
      Check (P.Valid (D),"valid descriptor");
      for Bad in 1 .. 8 loop
         declare Invalid : P.Descriptor := D; begin
            case Bad is
               when 1 => Invalid.Register := A.No_Register;
               when 2 => Invalid.Epoch := 0;
               when 3 => Invalid.Geometry.Region.Length := 0;
               when 4 => Invalid.Geometry.Region.Base := Unsigned_64'Last;
               when 5 => Invalid.Geometry.Byte_Offset := 16;
               when 6 => Invalid.Geometry.Region.Base := 16#1001#;
               when 7 => Invalid.Geometry.Region.Space := 1;
               when 8 => Invalid.Writable := False;
            end case;
            P.Install (IO,Invalid,OK); Check (not OK and then Calls = 0,"invalid descriptor no IPC");
         end;
      end loop;
      for S in Slot loop
         Config := (Tag => 17, Base => 16#1000#, Length => 16, Space => ACPI_Region_Policy.Memory_Space,
           Readable => True, Writable => True, Widths => [others => True],
           Register_Authority => (Resource_ID => Unsigned_64 (IDs (S)), Readable => True,
             Writable => True, Delegable => False, Write_Mask => Unsigned_64'Last),
           Register_Offset => (Unsigned_64 (S) - 1) * 8, Register_Width => T.Qword_Access);
         ACPI_Region_Policy.Install (Regions (S),Config,OK); Check (OK,"history backend install");
         D.Register := IDs (S); D.Geometry.Byte_Offset := Config.Register_Offset;
         P.Install (IO,D,OK); Check (OK,"history association");
         Targets (S) := (P.Identity (IO),7,1,IDs (S));
      end loop;
      Item := (Destination => Targets (1), For_Write => True, Value => 16#1234#);
      Unknown_Write := 0;
      P.Dispatch (IO,Item,Reply); Saved := P.Uncertain (IO);
      Check (Reply.Status = A.Indeterminate and then Saved.Present and then P.Pending (IO),"first history unknown");
      Wrong := Saved; Wrong.Operation := Wrong.Operation + 1;
      P.Confirm_Completion (IO,Wrong,True,OK); Check (not OK,"wrong outer operation");
      P.Confirm_Completion (Other,Saved,True,OK); Check (not OK,"other provider cannot confirm");
      P.Confirm_Completion (IO,Saved,False,OK); Check (not OK,"no completion evidence");
      P.Revoke_Register (IO,Targets (1),OK); Check (OK and then P.Pending (IO),"local revoke during unknown");
      D.Register := IDs (1); D.Geometry.Byte_Offset := 0; D.Epoch := 2;
      P.Install (IO,D,OK); Check (not OK,"no replacement while pending");
      Confirm_Model (IO,Saved,OK); Check (OK and then not P.Pending (IO),"transfer first inner fact");
      P.Reconciled (IO,Saved,OK); Check (OK,"outer confirmation retained");
      P.Describe (IO,Targets (1),Geometry,OK); Check (not OK,"confirmation cannot restore revoked association");
      Item.Destination := Targets (2); Unknown_Write := 8;
      P.Dispatch (IO,Item,Reply); Second := P.Uncertain (IO);
      Check (Reply.Status = A.Indeterminate and then Second.Present
        and then Second.Operation /= Saved.Operation,"new unknown after old confirmation");
      P.Reconciled (IO,Saved,OK); Check (OK,"old fact during later pending");
      Confirm_Model (IO,Second,OK); Check (OK,"transfer second inner fact");
      Before := Calls;
      P.Authorize (IO,Targets (2),True,OK); Check (not OK,"history capacity preflight");
      P.Dispatch (IO,Item,Reply); Check (Reply.Status = A.Denied and then Calls = Before,"history full no effects");
      P.Retire_Confirmation (IO,Saved,False,OK); Check (not OK,"arena must consume history");
      P.Retire_Confirmation (IO,Wrong,True,OK); Check (not OK,"wrong fact cannot retire");
      P.Retire_Confirmation (IO,Saved,True,OK); Check (OK,"retire exact first fact");
      P.Reconciled (IO,Saved,OK); Check (not OK,"retired first fact absent");
      P.Reconciled (IO,Second,OK); Check (OK,"second fact survives retirement");
      D.Epoch := 1; P.Install (IO,D,OK); Check (not OK,"same epoch cannot revive");
      D.Epoch := 2;
      Config := ACPI_Region_Policy.Policy (Regions (1));
      ACPI_Region_Policy.Install (Regions (1),Config,OK); Check (OK,"backend newer epoch");
      P.Install (IO,D,OK); Check (OK,"association newer epoch");
      P.Describe (IO,Targets (1),Geometry,OK); Check (not OK,"old binding stays stale");
      Targets (1).Epoch := 2;
      Item := (Destination => Targets (1), For_Write => False, Value => 0);
      P.Dispatch (IO,Item,Reply);
      Check (Reply.Status = A.Completed and then Model.Writes = 2,"new access without old write replay");
      P.Reconciled (IO,Second,OK); Check (OK,"old other-register fact independent of new access");
      P.Retire_Confirmation (IO,Second,True,OK); Check (OK,"retire second fact");
   end Exercise_History;
   procedure Exercise_Backend_Boundary is
      IO : P.Context;
      Config : ACPI_Region_Policy.Configuration :=
        (Tag => 17, Base => 16#1000#, Length => 16, Space => ACPI_Region_Policy.Memory_Space,
         Readable => True, Writable => False, Widths => [others => True],
         Register_Authority => (Resource_ID => 16#F123_4567_89AB_CDEF#, Readable => True,
           Writable => False, Delegable => False, Write_Mask => 0),
         Register_Offset => 0, Register_Width => T.Qword_Access);
      D : P.Descriptor :=
        (Register => A.Register_ID (Config.Register_Authority.Resource_ID), Epoch => 1,
         Geometry => (Region => (Space => 0, Base => Config.Base, Length => Config.Length),
           Byte_Offset => 0, Width => T.Qword_Access), Readable => True, Writable => True,
         Exact_Write_Safe => True, Write_Mask => Unsigned_64'Last);
      Target : A.Target;
      Reply : A.Response;
      Geometry : AML_Field_Layout.Register_Geometry;
      OK : Boolean;
   begin
      Model := (others => <>); Calls := 0; Unknown_Read := -1; Unknown_Write := -1;
      Revoke_On_Read := -1; Writes_Revoked := False;
      declare Fresh : States; begin Regions := Fresh; end;
      ACPI_Region_Policy.Install (Regions (1),Config,OK); Check (OK,"readonly backend");
      P.Initialize (IO,1,7,OK); Check (OK,"boundary context");
      P.Install (IO,D,OK); Check (OK,"advisory writable association");
      Target := (P.Identity (IO),7,1,D.Register);
      P.Authorize (IO,Target,True,OK); Check (OK,"local preflight is advisory");
      P.Dispatch (IO,(Target,True,1),Reply);
      Check (Reply.Status = A.Denied and then Calls = 1 and then Model.Writes = 0
        and then Model.Reads = 0 and then not P.Pending (IO),"backend authority independent of cache");
      P.Revoke_Register (IO,Target,OK); Check (OK,"replace metadata after revoke");
      Config := ACPI_Region_Policy.Policy (Regions (1));
      ACPI_Region_Policy.Revoke (Regions (1));
      ACPI_Region_Policy.Install (Regions (1),Config,OK); Check (OK,"backend epoch advances");
      D.Epoch := 2; D.Geometry.Width := T.Byte_Access; D.Write_Mask := 16#FF#;
      P.Install (IO,D,OK); Check (OK,"byte metadata for width validation");
      Target.Epoch := 2;
      P.Dispatch (IO,(Target,False,0),Reply);
      Check (Reply.Status = A.Failed_Completed and then not P.Pending (IO)
        and then P.Completed_Fault (IO).Present and then Model.Reads = 1,"completed out-of-width read");
      P.Describe (IO,Target,Geometry,OK); Check (not OK,"bad read disables association");
   end Exercise_Backend_Boundary;
   procedure Exercise_Exact_Layout is
      package L renames AML_Field_Layout;
      Field : L.Field_Geometry :=
        (Region => (Space => L.Memory_Space,Base => 16#1000#,Length => 16),
         Bit_Offset => 0,Bit_Length => 64,Flags => 4,Effective_Access => 4,others => <>);
      Register : L.Register_Geometry := (Region => Field.Region,Byte_Offset => 0,Width => T.Qword_Access);
   begin
      Check (L.Admitted (Field,Register),"exact memory Qword admission");
      Field.Region.Space := L.IO_Space; Register.Region := Field.Region;
      Check (not L.Admitted (Field,Register),"IO Qword rejected");
      Field.Region.Space := L.Memory_Space; Register.Region := Field.Region;
      Field.Bit_Offset := 64; Register.Byte_Offset := 8;
      Check (L.Admitted (Field,Register),"second exact memory Qword");
      Field.Bit_Offset := 8; Register.Byte_Offset := 1;
      Check (not L.Admitted (Field,Register),"misaligned exact Qword rejected");
      Field.Bit_Offset := 0; Register.Byte_Offset := 0;
      Field.Region.Length := 7; Register.Region := Field.Region;
      Check (not L.Admitted (Field,Register),"short Qword extent rejected");
      Field.Region.Length := 16; Field.Region.Base := Unsigned_64'Last - 7;
      Register.Region := Field.Region;
      Check (not L.Admitted (Field,Register),"wrapping Qword extent rejected");
      Field.Region.Base := 16#1000#; Register.Region := Field.Region;
      Field.Effective_Access := 0;
      Check (not L.Admitted (Field,Register),"AnyAcc never widens to Qword");
      Field.Effective_Access := 4; Field.Flags := 16#14#;
      Check (not L.Admitted (Field,Register),"locked Qword remains unsupported");
      Field.Flags := 4; Field.Has_Connection := True;
      Check (not L.Admitted (Field,Register),"connected Qword remains unsupported");
      Field.Has_Connection := False;
      for Form in L.Field_Form range L.Bank_Field .. L.Index_Field loop
         Field.Form := Form;
         Check (not L.Admitted (Field,Register),"selector Qword remains unsupported");
      end loop;
   end Exercise_Exact_Layout;
   Offsets : constant array (Positive range 1 .. 3) of Natural := [0,7,63];
begin
   Exercise_Exact_Layout;
   -- Exact single-register APIs share the full-width datum path: narrow AML
   -- revisions return an eight-byte Buffer, while wide revisions return Integer.
   for Mode in Binding_Mode range Exact_Single .. Exact_Set loop
      for Revision in Unsigned_8 range 1 .. 2 loop
         for Update in Rule loop
            for Source in Source_Kind loop
               Exercise (Revision,0,Update,Source,Mode => Mode);
            end loop;
            Exercise (Revision,0,Update,Pattern_Value,Fault => 4,Mode => Mode);
            Exercise (Revision,0,Update,Pattern_Value,Fault => 7,Mode => Mode);
            Exercise (Revision,0,Update,Pattern_Value,Fault => 8,Mode => Mode);
         end loop;
      end loop;
   end loop;
   for Revision in Unsigned_8 range 1 .. 2 loop
      for First of Offsets loop
         for Update in Rule loop
            for Source in Source_Kind loop Exercise (Revision,First,Update,Source); end loop;
         end loop;
      end loop;
   end loop;
   for Update in Rule loop
      for Policy in 1 .. 4 loop Exercise (2,7,Update,Pattern_Value,Policy => Policy); end loop;
      Exercise (2,0,Update,Pattern_Value,Policy => 1);
   end loop;
   for Fault in 1 .. 6 loop
      if Fault /= 3 then Exercise (2,7,Preserve,Pattern_Value,Fault => Fault); end if;
   end loop;
   Exercise_History;
   Exercise_Backend_Boundary;
   Ada.Text_IO.Put_Line ("Remote Field checks" & Checks'Image);
end Remote_Field_Tests;
