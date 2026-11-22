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
with CuBit.Messages;
with CuBit.Control_Events;
with CuBit.Grant_References;
with System.Storage_Elements;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure Owner_Tests is
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package O renames ACPI_Host_Bridge_Owners; package B renames O.B;
   package R renames B.RC;
   use type O.Phase, O.Failure, O.Step_Result, O.W.Word, Q.Outcome;
   use type Bytes, Byte;
   use type CuBit.Grant_References.Reference;
   use type ACPI_Native_Server.Owner_Outcome;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Image (Width : Integer_Width; Multiple : Boolean := False; Identity_Mode : Natural := 0) return Bytes is
      Code : constant Bytes := Block ([16#10#], Enc ("_SB_") & Block ([16#5B#,16#82#], Enc ("HST0")
        & [16#08#] & Enc ("_ADR") & [0]
        & (if Identity_Mode = 1 then
             [16#08#] & Enc ("_CID") & [16#0D#] & Enc ("PNP0A08") & [0]
           elsif Identity_Mode = 2 then
             [16#08#] & Enc ("_HID") & [16#0D#] & Enc ("VND0001") & [0]
             & [16#08#] & Enc ("_CID") & [16#0D#] & Enc ("PNP0A08") & [0]
           elsif Identity_Mode = 3 then
             [16#08#] & Enc ("_HID") & [16#0D#] & Enc ("PNP0A08") & [0]
             & Method ("_CID", [16#A4#, 16#72#, 16#60#, 16#00#, 16#00#])
           else [16#08#] & Enc ("_HID") & [16#0D#] & Enc ("PNP0A08") & [0])
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
   procedure Scenario (Width : Integer_Width; Mode : Positive; Identity_Mode : Natural := 0) is
      Data : constant Bytes := Image (Width, Mode = 18, Identity_Mode);
      Owner : aliased Q.State (1, Data'Length, Data'Length, 1, 0,
        Value_Object_Capacity => 256, Value_Byte_Capacity => 2048, Value_Element_Capacity => 128);
      type Owner_Access is access all Q.State;
      Ready, Clock_OK, Open : Boolean := True;
      Us, Ms : O.W.Word := 10;
      Begins, Ends, Evaluations : Natural := 0;
      Assigned : Natural := 0;
      Client_Memory : aliased Bytes (1 .. 128 * R.Page_Bytes) := [others => 0] with Alignment => 4096;
      Server_Memory : aliased Bytes (1 .. R.Server_Pages * R.Page_Bytes) := [others => 0] with Alignment => 4096;
      Second_Client : aliased Bytes (1 .. 128 * R.Page_Bytes) := [others => 0] with Alignment => 4096;
      Second_Server : aliased Bytes (1 .. R.Server_Pages * R.Page_Bytes) := [others => 0] with Alignment => 4096;
      Second_Own : constant CuBit.Grant_References.Reference := (slot => 17, generation => 29);
      Second_Peer : constant CuBit.Grant_References.Reference := (slot => 19, generation => 31);
      Remote : R.Server;
      Request : R.Queues.Submission;
      Got, Rung, Handled : Boolean;
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
         if Mode = 7 then Result := O.Quiet_Refusal; else Result := O.Opened; end if;
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
      procedure Maintain_Normal (Deadline : out O.W.Word; May_Continue : out Boolean) is
      begin Deadline := 9000; May_Continue := True; end Maintain_Normal;
      function Diagnostics_Normal (Server : Q.State) return Boolean is (Q.Valid (Server));
      package Scheduler is new ACPI_Native_Server.With_Host_Bridges
        (Manager, Maybe_Start, Normal, Power_Only, Power_Active, Maintain_Normal, Diagnostics_Normal, Maintain_Normal);
      Schema : constant CuBit.Protocols.Schema_Contract :=
         (Identity => B.M.Schema, Version => 1, Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => B.M.Message_Bytes);
      Reply : Q.Response; Result : O.Step_Result; Released : Boolean;
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
      if Mode = 17 then
         Manager.Request_Stop; Advance;
         Check (Manager.Retired and not Manager.Observe.Attempted, "stop before start has no obligations and cannot start later");
         Q.Release_Storage (Owner, Released); Check (Released, "release unused hosted fixture"); return;
      end if;
      Ready := Mode /= 2; Clock_OK := Mode /= 3;
      Manager.Start (Owner'Unchecked_Access, CuBit.Handles.Model_Endpoint, B.P.From_Word (16#DEAD_BEEF_1234_0001#), Schema, 1,
         1000, 1000, (if Mode = 4 then 1 else 1000), 1, 1000, 20);
      if Mode = 2 then Check (Manager.Observe.Why = O.Metadata_Refused, "metadata gate refusal");
      elsif Mode = 3 then Check (Manager.Observe.Why = O.Clock_Failed, "initial missing clock"); Clock_OK := True;
      else Check (Manager.Blocks_AML, "capture excludes other AML"); end if;
      if Mode = 1 then Manager.Request_Stop;
      elsif Mode = 5 then Us := 1000; Ms := 1000;
      elsif Mode not in 2 | 3 then
         for Turn in 1 .. 100 loop
            Advance;
            exit when Manager.Observe.Stage /= O.Capturing;
         end loop;
         Check (not Manager.Blocks_AML, "metadata claim released before queries");
         if Mode = 6 then
            Advance; Check (Manager.Blocks_AML, "query window excludes peers"); Manager.Request_Stop;
         elsif Mode in 9 | 11 .. 16 | 18 .. 21 then
            for Turn in 1 .. 100 loop
               Advance;
               exit when Q.Retained_Results_Pending (Owner);
            end loop;
            Check (Q.Retained_Results_Pending (Owner) and Begins = 3, "actual root buffer retained after two predefined nonroots");
            if Mode = 9 then Manager.Request_Stop;
            else
               for Turn in 1 .. 4 loop
                  Advance; exit when Manager.Observe.Stage = O.Waiting_Open;
               end loop;
               Check (Manager.Observe.Stage = O.Waiting_Open and not Manager.Blocks_AML
                 and not Q.Retained_Results_Pending (Owner), "async open follows query and window release");
               if Mode in 12 | 16 then Manager.Request_Stop; end if;
               Scheduler.Handle_Completion (Owner, (token => CuBit.Channels.Last_Token, from => 16#DEAD_BEEF_1234_0001#,
                 status => (if Mode in 14 | 16 then CuBit.Messages.COMPLETION_TARGET_DIED
                            elsif Mode = 15 then CuBit.Messages.COMPLETION_CANCELLED
                            else CuBit.Messages.COMPLETION_OK), valid => True, others => <>), Handled);
               Check (Handled, "original async completion routed");
               if Mode in 14 .. 16 then
                  Advance;
                  Check (Manager.Current_Transport.Completion_Status /= CuBit.Messages.COMPLETION_OK
                    and not Manager.Current_Transport.Published,
                    "terminal kernel completion never becomes a report publication");
                  Check (Manager.Observe.Had_Errors and Manager.Observe.Why = O.Transport_Refused,
                     "known setup failure is explicit while serving or stopping");
               else
                  R.Attach_Server (Remote, Server_Memory'Address, Client_Memory'Address);
                  Advance;
                  R.Take (Remote, Request, Got, Rung);
                  if Mode = 12 then
                  Check (not Got, "stop before receipt never publishes ready channel");
                  else
                  Check (Got and Manager.Observe.Stage = O.Waiting_Receipt, "one report published through actual rings");
                  if Mode = 13 then Manager.Request_Stop; end if;
                  if Mode = 21 then
                     Scheduler.Maintain (Wake, Healthy);
                     Check (Healthy and Wake = 1000, "pending receipt keeps original discovery deadline");
                  end if;
                  R.Complete (Remote, Request.Tag, [B.M.Root_ID => Request.Item (B.M.Root_ID),
                    B.M.Sequence => Request.Item (B.M.Sequence), B.M.Disposition => B.M.Acceptance_Code (B.M.Accepted)], Rung);
                  if Mode = 21 then
                     Scheduler.Maintain (Wake, Healthy);
                     Check (Healthy and Wake = 0, "ready receipt cannot be lost between poll and wait arm");
                  end if;
                  Advance;
                  end if;
                  R.Detach_Server (Remote);
               end if;
               Check (not Manager.Retired, "owned grant remains an obligation");
               if Mode in 11 | 19 | 21 then
                  Check (Manager.Observe.Stage = O.Holding and Manager.Observe.Held_Sessions = 1
                    and Manager.Observe.Discovery_Finished and not Manager.Blocks_AML,
                    "accepted session stays alive after discovery without holding Core");
                  Check (not Manager.Observe.Had_Errors and Manager.Observe.Why = O.None,
                    "successful discovery does not classify ordinary nonroots as errors");
                  Check (CuBit.Channels.Close_Calls = 0, "acknowledgment does not end producer session");
                  Us := 5000; Ms := 5000; Clock_OK := False;
                  Advance;
                  Check (Manager.Observe.Stage = O.Holding and Manager.Observe.Held_Sessions = 1
                    and Manager.Wake_Deadline = CuBit.Messages.Wait_Forever and CuBit.Channels.Close_Calls = 0,
                    "completed discovery deadline and unavailable clock do not expire accepted session");
                  Clock_OK := True;
                  if Mode = 19 then
                     Manager.Note_Event ((Kind => CuBit.Control_Events.Grant_Revoked,
                       Slot => CuBit.Channels.Peer_Reference.slot, Generation => CuBit.Channels.Peer_Reference.generation,
                       Peer => B.P.From_Word (16#D0AD_BEEF_1234_0001#)), Handled);
                     Check (not Handled and not Manager.Observe.Stop_Requested,
                       "wrong full-width peer cannot end a parked session");
                     Manager.Note_Event ((Kind => CuBit.Control_Events.Grant_Revoked,
                       Slot => CuBit.Channels.Peer_Reference.slot, Generation => CuBit.Channels.Peer_Reference.generation,
                       Peer => B.P.From_Word (16#DEAD_BEEF_1234_0001#)), Handled);
                     Check (Handled and Manager.Observe.Stop_Requested and Manager.Observe.Why = O.Transport_Ended,
                       "parked accepted-session end starts known cleanup");
                  else Manager.Request_Stop; end if;
               elsif Mode = 20 then
                  Check (Manager.Observe.Held_Sessions = 1 and Power_Turns = 0,
                    "saved power request begins while accepted bridge session still exists");
                  Power_Blocks := True; Advance;
                  Check (not Manager.Retired and Power_Turns = 0 and CuBit.Channels.Close_Calls = 1,
                    "no hardware entry before original owned region retirement");
               elsif Mode = 18 then
                  Check (Manager.Observe.Held_Sessions = 1 and CuBit.Channels.Close_Calls = 0,
                    "first root session survives while another candidate remains");
                  declare First_Copy : constant Bytes := Client_Memory; begin
                     CuBit.Channels.Own_Base := O.W.Word (System.Storage_Elements.To_Integer (Second_Client'Address));
                     CuBit.Channels.Peer_Base := O.W.Word (System.Storage_Elements.To_Integer (Second_Server'Address));
                     CuBit.Channels.Next_Own_Reference := Second_Own;
                     CuBit.Channels.Next_Peer_Reference := Second_Peer;
                     for Turn in 1 .. 10 loop Advance; exit when Manager.Observe.Stage = O.Waiting_Open; end loop;
                     Check (Manager.Observe.Stage = O.Waiting_Open and CuBit.Channels.Open_Calls = 2,
                       "second root queries and opens independently");
                     Manager.Complete_Open ((token => CuBit.Channels.Last_Token,
                       from => 16#DEAD_BEEF_1234_0001#, valid => True, others => <>), Handled);
                     Check (Handled, "second original setup completion routed");
                     R.Attach_Server (Remote, Second_Server'Address, Second_Client'Address);
                     Advance; R.Take (Remote, Request, Got, Rung);
                     Check (Got, "second root request reaches its own actual ring");
                     R.Complete (Remote, Request.Tag, [B.M.Root_ID => Request.Item (B.M.Root_ID),
                       B.M.Sequence => Request.Item (B.M.Sequence), B.M.Disposition => B.M.Acceptance_Code (B.M.Accepted)], Rung);
                     Advance; R.Detach_Server (Remote);
                     Check (Manager.Observe.Stage = O.Holding and Manager.Observe.Held_Sessions = 2
                       and Manager.Observe.Acknowledged = 2 and CuBit.Channels.Close_Calls = 0,
                       "both acknowledged root sessions remain live after discovery");
                     Check (Client_Memory = First_Copy, "second root cannot overwrite first root snapshot or ring");
                  end;
                  Manager.Request_Stop;
               end if;
               CuBit.Memory_Grants.Model_Retired := True;
            end if;
         elsif Mode in 7 | 8 | 10 then null;
         else Manager.Request_Stop;
         end if;
      end if;
      for Turn in 1 .. 100 loop
         exit when Manager.Retired;
         Advance;
      end loop;
      Check (Manager.Retired and not Manager.Blocks_AML, "known owner retires");
      Check (not Q.Retained_Results_Pending (Owner), "no retained AML left");
      Check (Normal_Turns > 0 or Mode in 1 .. 6 | 8 | 9 | 10 | 17, "normal work resumes at quiet boundaries");
      Check (Power_Turns = (if Mode = 20 then 1 else 0), "power callback runs only after actual bridge retirement");
      if Mode in 9 | 11 .. 16 | 18 .. 21 then
         Check (Begins = (if Mode = 18 then 4 else 3) and Ends = Begins, "each actual window closes once");
         Check (CuBit.Channels.Open_Calls = (if Mode = 9 then 0 elsif Mode = 18 then 2 else 1), "one transport per identified root");
         if Mode = 18 then
            Check (CuBit.Channels.Close_Calls = 2 and CuBit.Memory_Grants.Last_Query = CuBit.Channels.Own_Reference,
              "stop retires both distinct original grants before freeing the owner");
         end if;
         if Mode in 11 | 13 then Check (Manager.Observe.Acknowledged = 1, "accepted report recorded exactly once, including stop drain"); end if;
      elsif Mode = 6 then Check (Begins = 1 and Ends = 1, "stop closes original query once");
      elsif Mode = 7 then Check (Begins = 3 and Ends = 0 and Evaluations = 0, "quiet refusals evaluate no AML");
      elsif Mode in 8 | 10 then Check (Begins = 0 and Evaluations = 0 and Manager.Observe.Why = O.Correlation_Refused, "correlation refusal precedes query");
      else Check (Begins = 0 and Evaluations = 0, "capture-only paths evaluate nothing"); end if;
      Manager.Start (Owner'Unchecked_Access, CuBit.Handles.Model_Endpoint, B.P.From_Word (1), Schema, 1, 2000, 2000, 1000, 2, 2000, 20);
      Check (Manager.Retired, "consumed manager cannot restart");
      Q.Release_Storage (Owner, Released); Check (Released, "release only retired owner");
   end Scenario;
begin
   for Width in Integer_Width loop for Mode in 1 .. 21 loop Scenario (Width, Mode); end loop; end loop;
   for Width in Integer_Width loop
      for Identity_Mode in 1 .. 3 loop Scenario (Width, 11, Identity_Mode); end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Host bridge manager checks" & Checks'Image);
end Owner_Tests;
