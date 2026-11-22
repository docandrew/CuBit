pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Native_Server.With_Host_Bridges;
with AML_Decode; use AML_Decode;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Host_Bridge_Owners;
with ACPI_Hosted_Heap;
with Root_Test_Support; use Root_Test_Support;
with CuBit.Handles;
with CuBit.Protocols;
with CuBit.Channels;
with CuBit.Memory_Grants;
with System.Storage_Elements;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure Retained_Tests is
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package O renames ACPI_Host_Bridge_Owners; package B renames O.B;
   package R renames B.RC;
   use type O.Phase, O.Step_Result, O.W.Word, Q.Outcome;
   use type Bytes, Byte;
   use type ACPI_Native_Server.Owner_Outcome;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Image (Width : Integer_Width; Multiple : Boolean := False) return Bytes is
      Code : constant Bytes := Block ([16#10#], Enc ("_SB_") & Block ([16#5B#,16#82#], Enc ("HST0")
        & [16#08#] & Enc ("_HID") & [16#0D#] & Enc ("PNP0A08") & [0]
        & Method ("_CRS", [16#A4#] & Block ([16#11#], [16#0A#,2,16#79#,0])))
        & (if Multiple then Block ([16#5B#,16#82#], Enc ("HST1")
            & [16#08#] & Enc ("_HID") & [16#0D#] & Enc ("PNP0A03") & [0]
            & Method ("_CRS", [16#A4#] & Block ([16#11#], [16#0A#,2,16#79#,0]))) else []));
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0]; Sum : Byte := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); for I in 0 .. 3 loop Data (5 + I) := Byte ((Data'Length / 256 ** I) mod 256); end loop;
      Data (9) := (if Width = Bits_32 then 1 else 2); Data (37 .. Data'Last) := Code;
      for X of Data loop Sum := Sum + X; end loop; Data (10) := 0 - Sum; return Data;
   end Image;
   procedure Scenario (Width : Integer_Width; Mode : Positive) is
      Data : constant Bytes := Image (Width, Mode = 18);
      Owner : aliased Q.State (1, Data'Length, Data'Length, 1, 0,
        Value_Object_Capacity => 256, Value_Byte_Capacity => 2048, Value_Element_Capacity => 128);
      type Owner_Access is access all Q.State;
      Ready : constant Boolean := True;
      Clock_OK, Open : Boolean := True;
      Us, Ms : constant O.W.Word := 10;
      Begins, Ends, Evaluations : Natural := 0;
      Assigned : Natural := 0;
      Client_Memory : aliased Bytes (1 .. 128 * R.Page_Bytes) := [others => 0] with Alignment => 4096;
      Server_Memory : aliased Bytes (1 .. R.Server_Pages * R.Page_Bytes) := [others => 0] with Alignment => 4096;
      Wake : O.W.Word;
      Healthy : Boolean;
      function Metadata_Ready return Boolean is (Ready);
      function Query_Ready return Boolean is (True);
      function Current return Boolean is (Open);
      procedure Clock (Microseconds, Milliseconds : out O.W.Word; Available : out Boolean) is
      begin Microseconds := Us; Milliseconds := Ms; Available := Clock_OK; end Clock;
      procedure Begin_Window (Server : in out Q.State; Deadline_Microseconds : O.W.Word; Result : out O.Window_Result) is
         pragma Unreferenced (Server, Deadline_Microseconds);
      begin
         Begins := Begins + 1; Open := True;
         if Mode = 23 then Result := O.Uncertain; else Result := O.Opened; end if;
      end Begin_Window;
      procedure End_Window (Server : in out Q.State; Healthy : Boolean; Closed : out Boolean; Now_Milliseconds : out O.W.Word) is
      begin
         Check (Healthy and not Q.Retained_Results_Pending (Server), "close after exact query release");
         Ends := Ends + 1; Closed := True; Open := False; Now_Milliseconds := Ms;
      end End_Window;
      procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
         Result : out C.Values.Result; Status : out C.Values.Access_Status) is
      begin Evaluations := Evaluations + 1; C.Evaluate_Named (Service, Item, Budget, Result, Status); end Evaluate;
      procedure Correlation (Device : C.Node_Descriptor; Root_ID, Sequence, Open_Token, Data_Token : out O.W.Word; Available : out Boolean) is
         pragma Unreferenced (Device);
      begin
         Assigned := Assigned + 1;
         Root_ID := (if Mode = 10 then 0 else O.W.Word (Assigned)); Sequence := 1;
         Open_Token := 100 + 2 * O.W.Word (Assigned); Data_Token := Open_Token + 1; Available := Mode /= 8;
      end Correlation;
      Codes : constant O.G.Reason_Codes := O.G.Canonical_Reasons;
      package Manager is new O.Managers (Owner_Access, Metadata_Ready, Query_Ready, Current,
         Clock, Begin_Window, End_Window, Evaluate, Correlation, Codes);
      Normal_Turns, Power_Turns : Natural := 0;
      Power_Blocks : Boolean := False;
      function Power_Active return Boolean is (Power_Blocks);
      procedure Maybe_Start (Server : in out Q.State) is
         pragma Unreferenced (Server);
      begin Check (not Manager.Observe.Attempted, "start hook only before consumed manager"); end Maybe_Start;
      procedure Normal (Server : in out Q.State; Outcome : out ACPI_Native_Server.Owner_Outcome) is
      begin
         Check (Q.Valid (Server) and not Manager.Blocks_AML and not Power_Blocks,
           "normal work never enters retained metadata or AML window");
         Normal_Turns := Normal_Turns + 1; Outcome := ACPI_Native_Server.Owner_Idle;
      end Normal;
      procedure Power_Only (Server : in out Q.State; Outcome : out ACPI_Native_Server.Owner_Outcome) is
      begin
         Check (Q.Valid (Server) and Manager.Retired, "hardware-entry work waits for original bridge retirement");
         Power_Turns := Power_Turns + 1; Outcome := ACPI_Native_Server.Owner_Idle;
      end Power_Only;
      Maintenance_Healthy : Boolean := True;
      procedure Maintain_Normal (Deadline : out O.W.Word; May_Continue : out Boolean) is
      begin Deadline := 9000; May_Continue := Maintenance_Healthy and not Manager.Blocks_AML; end Maintain_Normal;
      procedure Maintain_Exclusive (Deadline : out O.W.Word; May_Continue : out Boolean) is
      begin
         Check (Manager.Blocks_AML, "exclusive maintenance requires actual AML ownership");
         Deadline := 7000; May_Continue := Maintenance_Healthy;
      end Maintain_Exclusive;
      function Diagnostics_Normal (Server : Q.State) return Boolean is (Q.Valid (Server));
      package Scheduler is new ACPI_Native_Server.With_Host_Bridges
        (Manager, Maybe_Start, Normal, Power_Only, Power_Active, Maintain_Normal, Diagnostics_Normal, Maintain_Exclusive);
      Schema : constant CuBit.Protocols.Schema_Contract :=
         (Identity => B.M.Schema, Version => 1, Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => B.M.Message_Bytes);
      Reply : Q.Response; Result : O.Step_Result;
      procedure Advance is
         Outcome : ACPI_Native_Server.Owner_Outcome;
      begin
         Scheduler.Advance (Owner, Outcome);
         Result := (case Outcome is when ACPI_Native_Server.Owner_Idle => O.Idle,
           when ACPI_Native_Server.Owner_Pending => O.Runnable, when ACPI_Native_Server.Owner_Refused => O.Failed);
         Check (Result /= O.Failed, "known lifecycle stays recoverable");
      end Advance;
   begin
      CuBit.Channels.Mode := CuBit.Channels.Good;
      CuBit.Channels.Open_Calls := 0; CuBit.Channels.Close_Calls := 0;
      CuBit.Channels.Submit_OK := True;
      CuBit.Channels.Next_Own_Reference := CuBit.Channels.Own_Reference;
      CuBit.Channels.Next_Peer_Reference := CuBit.Channels.Peer_Reference;
      CuBit.Channels.Own_Base := O.W.Word (System.Storage_Elements.To_Integer (Client_Memory'Address));
      CuBit.Channels.Peer_Base := O.W.Word (System.Storage_Elements.To_Integer (Server_Memory'Address));
      CuBit.Memory_Grants.Model_Retired := False;
      Q.Handle (Owner, Q.Snapshot_Provider, (Label => Q.Start_Snapshot, Data => [Q.Revision (Owner),1,0,0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "start actual Requests owner");
      Q.Import_Block (Owner, Q.Snapshot_Provider, Q.Revision (Owner), 1, C.DSDT, Data, Reply); Check (Reply.Status = Q.OK, "import fixture");
      Q.Handle (Owner, Q.Snapshot_Provider, (Label => Q.Finish_Snapshot, Data => [Q.Revision (Owner),0,0,0], others => <>), Reply);
      Check (Reply.Status = Q.OK and Q.Device_Walk_Finished (Owner), "actual private Core ready");
      Manager.Start (Owner'Unchecked_Access, CuBit.Handles.Model_Endpoint, B.P.From_Word (16#DEAD_BEEF_1234_0001#), Schema, 1,
         1000, 1000, 1000, 1, 1000, 20);
      for Turn in 1 .. 100 loop
         exit when Manager.Observe.Stage in O.Waiting_Open | O.Retained;
         Advance;
         Scheduler.Maintain (Wake, Healthy);
         Check (Healthy, "active bridge window uses exclusive maintenance instead of Idle-only normal maintenance");
      end loop;
      if Mode = 22 then
         Check (Manager.Observe.Stage = O.Waiting_Open and not Manager.Blocks_AML,
            "transport starts only after query owner release");
         Clock_OK := False;
         Manager.Advance (Result);
      end if;
      Check (Manager.Observe.Stage = O.Retained and not Manager.Retired,
         "uncertain resources remain retained");
      Check (Manager.Blocks_AML = (Mode = 23), "retained query excludes AML; retained transport does not");
      Check (Scheduler.Admission_Open = (Mode = 22), "ordinary request admission follows retained AML ownership");
      Scheduler.Maintain (Wake, Healthy);
      Check (Healthy and Wake = (if Mode = 23 then 7000 else 9000), "publication retention does not stop service or replace normal deadline");
      Check (Scheduler.Diagnostics_Quiet (Owner) = (Mode = 22), "logs follow actual AML-owner quietness");
      Maintenance_Healthy := False;
      Scheduler.Maintain (Wake, Healthy);
      Check (not Healthy, "genuine hardware or normal-owner maintenance failure is not hidden");
      Maintenance_Healthy := True;
      declare
         Before : constant Natural := Normal_Turns;
         Opens : constant Natural := CuBit.Channels.Open_Calls;
         Queries : constant Natural := Evaluations;
         Outcome : ACPI_Native_Server.Owner_Outcome;
      begin
         for Turn in 1 .. 3 loop
            Scheduler.Advance (Owner, Outcome);
            Check (Outcome = ACPI_Native_Server.Owner_Idle, "retained publication never stops serving or spins runnable");
         end loop;
         Check (Normal_Turns = Before + (if Mode = 22 then 3 else 0), "normal work resumes only without retained AML");
         Check (CuBit.Channels.Open_Calls = Opens and Evaluations = Queries,
            "quarantine never reopens or reevaluates the publication");
         Power_Blocks := True;
         Scheduler.Advance (Owner, Outcome);
         Check (Outcome = ACPI_Native_Server.Owner_Idle and Power_Turns = 0,
            "power hardware entry stays blocked by unretired original resources");
         Scheduler.Drain (Owner, Outcome);
         Check (Outcome = ACPI_Native_Server.Owner_Refused and not Manager.Retired,
            "explicit stop still reports unretired original resources");
      end;
   end Scenario;
begin
   for Width in Integer_Width loop
      for Mode in 22 .. 23 loop Scenario (Width, Mode); end loop;
   end loop;
   Ada.Text_IO.Put_Line ("retained publication checks" & Checks'Image);
end Retained_Tests;
