pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Observer_Loans;
with ACPI_Query_Sessions;
with ACPI_Requests;
with ACPI_Requests.Passive_Queries;
with ACPI_Service;
with ACPI_Protocol;
with ACPI_Passive_Query.Fields;
with ACPI_Passive_Query.Replies;
with Firmware_Tables;
procedure Query_Owner_Bridge_Tests is
   package Q renames ACPI_Requests;
   package Owner renames Q.Passive_Queries;
   package B renames Owner.Data;
   package W renames B.Wire;
   package P renames B.Pages;
   package R renames ACPI_Passive_Query.Replies;
   package F renames ACPI_Passive_Query.Fields;
   package IO renames Ada.Streams.Stream_IO;
   use type Ada.Streams.Stream_Element_Offset;
   use type Q.Outcome, Q.Phase;
   use type W.Word, W.Instance_ID, W.Generation, W.Node_Index;
   use type W.Query_Status, W.Field_Status, W.Operation;
   use type B.Dispatcher.Outcome;
   use type P.Byte;
   use type R.U32;
   use type F.Field_Name, F.Value_Kind;
   Checks : Natural := 0;
   Packet_Bytes : constant := 4_096;
   Scratch_Bytes : constant := 256;
   Maximum_Visits : constant Positive := ACPI_Service.Max_Namespace_Nodes;
   Snapshot_Mutations : constant Q.Revision_Number := 3;
   Resource_Item_Budget : constant B.Dispatcher.Resources.Data_IO.Item_Count := 32;
   Node_Limit : constant W.Node_Limit := W.Node_Limit (Maximum_Visits);
   Own_Instance : constant W.Instance_ID := 16#F123_4567_89AB_CDEF#;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Why; end if;
   end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      Check (Ada.Command_Line.Argument_Count = 2, "pinned fixture and independent lifecycle case");
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in IO.Count (Firmware_Tables.Table_Header_Size) .. Packet_Bytes,
        "fixture extent before conversion");
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Last = Raw'Last, "exact table input");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   procedure Scenario (Initial : Q.Revision_Number) is
      Server : Q.State (1, Table'Length, Table'Length, 1, Initial);
      Reply : Q.Response;
      Request, Response, Filter, Payload : P.Byte_Array (0 .. Packet_Bytes - 1);
      Nodes : B.Search.Node_Array (1 .. 4);
      Parents : B.Paths.Node_Array (1 .. 16);
      Reader_Data, Text_Source : Firmware_Tables.Bytes (1 .. Scratch_Bytes);
      Atoms : B.Reader.Atoms (1 .. 8);
      Result : B.Dispatcher.Report;
      Item : W.Request := ((Kind => W.Find_By_ID, ID => W.From_Text ("PNP0C50").Value,
        others => <>), (Present => False));
      Wanted_Generation : W.Generation;
      Device, Method_Device, Bus_Device : W.Node_Ref;
      procedure Build is
         Accepted : Boolean;
         Written : W.Word;
      begin
         Check (W.Valid (Item, Node_Limit), "test query metadata valid " & Item.Key.Kind'Image);
         P.Encode_Request (Item, [], Request, Node_Limit, Written, Accepted);
         Check (Accepted and then Written >= P.Frame_Byte_Count, "canonical owned query");
      end Build;
      procedure Invoke (Stamp : ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
                        Visits : Positive := Maximum_Visits) is
         Before : constant Q.Revision_Number := Q.Revision (Server);
         Phase : constant Q.Phase := Q.Current (Server);
         Tables : constant Natural := Q.Observe (Server).Tables;
      begin
         Build;
         Owner.Handle (Server, Own_Instance, Stamp, Request, Visits, Resource_Item_Budget,
           Nodes, Parents, Reader_Data, Atoms, Text_Source, Filter, Payload, Response, Result);
         Check (Q.Revision (Server) = Before and then Q.Current (Server) = Phase
           and then Q.Observe (Server).Tables = Tables, "query preserves revision/phase/catalog");
         if Result.Status = B.Dispatcher.Reply_Ready then
            Check (R.Read_Envelope (Response, Node_Limit).Accepted, "actual reply framing");
         else
            Check (Result.Written = 0 and then (for all Byte of Response => Byte = 0),
              "refusal exposes no partial reply");
         end if;
      end Invoke;
      procedure Ready_Reply (Status : W.Query_Status) is
      begin
         Check (Result.Status = B.Dispatcher.Reply_Ready, "typed query reply");
         case Result.Selected is
            when W.Find_By_ID | W.Find_By_Address =>
               Check (Result.Find_Result.Header.Status = Status, "find status from owned state");
            when W.Describe => Check (Result.Describe_Result.Header.Status = Status, "describe status");
            when W.Resources => Check (Result.Resources_Result.Header.Status = Status, "resource status");
         end case;
      end Ready_Reply;
      procedure Target (Kind : W.Operation; Ref : W.Node_Ref) is
      begin
         Item := ((Kind => Kind, Instance => Ref.Instance, Revision => Ref.Revision,
           Node => Ref.Index, others => <>), (Present => False));
      end Target;
      procedure Transport_Checks is
         type Peer_ID is new W.Word;
         type Loan_Reference is record Slot, Generation : W.Word := 0; end record;
         No_Reference : constant Loan_Reference := (0, 0);
         Expected_Peer : constant Peer_ID := 16#FEDC_BA98_7654_3210#;
         Expected_Reference : constant Loan_Reference := (16#1234_5678#, 16#ABCD_9876_5432_1010#);
         type Mapping_Token is new W.Word;
         Expected_Mapping : constant Mapping_Token := 16#FFFF_1234_5678_ABCD#;
         type Reply_Token is new W.Word;
         Expected_Reply : constant Reply_Token := 16#FEDC_1234_ABCD_5678#;
         type Failure is (None, Read_Error, Write_Error);
         type Context is record
            Memory : P.Byte_Array (0 .. Packet_Bytes - 1) := [others => 0];
            Events : String (1 .. 64) := [others => ' '];
            Used : Natural := 0;
            Active, Saved, Return_Fails, Answer_Fails, Save_Fails : Boolean := False;
            Fail : Failure := None;
            Evaluations, Answers : Natural := 0;
            Dispatch : B.Dispatcher.Report;
         end record;
         procedure Event (C : in out Context; Value : Character) is
         begin C.Used := C.Used + 1; C.Events (C.Used) := Value; end Event;
         procedure Acquire (C : in out Context; Loan : Loan_Reference; Expected_Owner : Peer_ID;
            Byte_Length : W.Word; Acquired : out Mapping_Token; Accepted : out Boolean) is
         begin
            Event (C, 'A');
            Check (not C.Active and then Byte_Length = Packet_Bytes, "owned loan exact extent and exclusive acquisition");
            Accepted := Loan = Expected_Reference and then Expected_Owner = Expected_Peer;
            Acquired := (if Accepted then Expected_Mapping else 0); C.Active := Accepted;
         end Acquire;
         procedure Copy_In (C : in out Context; Acquired : Mapping_Token;
            Data : out P.Byte_Array; Accepted : out Boolean) is
         begin
            Event (C, 'I');
            Check (C.Active and then Acquired = Expected_Mapping and then Data'Length = Packet_Bytes,
              "owned request copies only the acquired exact extent");
            Data := C.Memory; Accepted := C.Fail /= Read_Error;
         end Copy_In;
         procedure Evaluate (C : in out Context; Request_Data : P.Byte_Array;
            Response_Data : out P.Byte_Array; Written, Required_Bytes : out W.Word; Accepted : out Boolean) is
         begin
            Event (C, 'E'); C.Evaluations := C.Evaluations + 1;
            -- Model the client changing mapped bytes after Copy_In. Dispatch
            -- must use its private copy, with readiness/revision from Server.
            C.Memory := [others => 0];
            Owner.Handle (Server, Own_Instance, ACPI_Protocol.Observer_Tag, Request_Data,
              Maximum_Visits, Resource_Item_Budget, Nodes, Parents, Reader_Data, Atoms,
              Text_Source, Filter, Payload, Response_Data, C.Dispatch);
            Written := C.Dispatch.Written; Required_Bytes := C.Dispatch.Required_Bytes;
            Accepted := C.Dispatch.Status = B.Dispatcher.Reply_Ready;
         end Evaluate;
         procedure Copy_Out (C : in out Context; Acquired : Mapping_Token;
            Data : P.Byte_Array; Accepted : out Boolean) is
         begin
            Event (C, 'O');
            Check (C.Active and then Acquired = Expected_Mapping and then Data'Length = Packet_Bytes,
              "owned response copies only the acquired exact extent");
            C.Memory := Data; Accepted := C.Fail /= Write_Error;
         end Copy_Out;
         procedure Return_Acquisition (C : in out Context; Loan : Loan_Reference;
            Acquired : Mapping_Token; Accepted : out Boolean) is
         begin
            Event (C, 'R');
            Check (C.Active and then Loan = Expected_Reference and then Acquired = Expected_Mapping,
              "owned session returns exact original acquisition");
            Accepted := not C.Return_Fails;
            if Accepted then C.Active := False; end if;
         end Return_Acquisition;
         package Loans is new ACPI_Observer_Loans
           (Context, Peer_ID, Loan_Reference, No_Reference, Mapping_Token, 0,
            Acquire, Copy_In, Evaluate, Copy_Out, Return_Acquisition);
         procedure Save (C : in out Context; Handle : out Reply_Token; Accepted : out Boolean) is
         begin
            Event (C, 'S'); Check (not C.Saved, "one saved reply authority");
            Accepted := not C.Save_Fails; C.Saved := Accepted;
            Handle := (if Accepted then Expected_Reply else 0);
         end Save;
         procedure Answer (C : in out Context; Handle : Reply_Token;
            Operation : Loans.Report; Delivered : out Boolean) is
         begin
            Event (C, 'P');
            Check (not C.Active and then C.Saved and then Handle = Expected_Reply
              and then Operation.May_Reply, "owned query return precedes saved reply on every outcome");
            C.Saved := False; C.Answers := C.Answers + 1; Delivered := not C.Answer_Fails;
         end Answer;
         package Sessions is new ACPI_Query_Sessions (Loans, Reply_Token, 0, Save, Answer);
         use type Sessions.Outcome, Loans.Outcome;
         Session : Sessions.State;
         C : Context;
         Input, Output : P.Byte_Array (0 .. Packet_Bytes - 1);
         Report : Sessions.Report;
         Before : constant Q.Revision_Number := Q.Revision (Server);
         procedure Build_Transport (Kind : W.Operation := W.Find_By_ID) is
            Accepted : Boolean;
            Written : W.Word;
         begin
            Check (Sessions.Ready (Session), "previous saved reply retired before next test request");
            C := (others => <>);
            if Kind = W.Find_By_ID then
               Item := ((Kind => Kind, ID => W.From_Text ("PNP0C50").Value, others => <>), (Present => False));
            elsif Kind = W.Find_By_Address then
               Item := ((Kind => Kind, Instance => Own_Instance, Revision => Wanted_Generation,
                 Node => Bus_Device.Index, Address => 16#15#, others => <>), (Present => False));
            else Target (Kind, Device); end if;
            P.Encode_Request (Item, [], C.Memory, Node_Limit, Written, Accepted);
            Check (Accepted and then Written = P.Frame_Byte_Count, "canonical owned transport request");
         end Build_Transport;
         procedure Begin_Request
           (Stamp : ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
            Peer : Peer_ID := Expected_Peer; Ref : Loan_Reference := Expected_Reference;
            Length : W.Word := Packet_Bytes) is
         begin
            Sessions.Begin_Request (Session, C, Stamp, Peer, Ref, Length, Input, Output, Report);
            Check (Sessions.Valid (Session) and then Q.Valid (Server) and then Q.Revision (Server) = Before,
              "transport preserves actual owner state and exact generation");
         end Begin_Request;
         procedure Trace (Value : String) is
         begin Check (C.Events (1 .. C.Used) = Value, "owned transport ordering " & Value); end Trace;
      begin
         for Kind in W.Operation loop
            Build_Transport (Kind); Begin_Request; Trace ("SAIEORP");
            Check (Report.Status = Sessions.Answered and then Report.Operation.Status = Loans.Completed
              and then C.Evaluations = 1 and then C.Answers = 1 and then Sessions.Ready (Session),
              "all actual owner query kinds pass through loan and saved reply lifecycle");
            declare H : constant R.Header_Result := R.Read_Envelope (C.Memory, Node_Limit); begin
               Check (H.Accepted and then H.Value.Status = W.OK and then H.Value.Context.Key.Kind = Kind,
                 "owned transport returns correct typed reply after client mutation");
               Check (R.Matches_Request (C.Memory, Input, Node_Limit), "owned transport reply correlates to private request");
            end;
         end loop;
         Build_Transport; Begin_Request (Stamp => 0); Trace ("");
         Check (Report.Status = Sessions.Denied and then Report.Immediate_Reply
           and then C.Evaluations = 0, "denied role neither saves nor acquires");
         Build_Transport; Begin_Request (Length => P.Frame_Byte_Count - 1); Trace ("");
         Check (Report.Status = Sessions.Invalid_Extent and then Report.Immediate_Reply,
           "short extent refused before saved reply/acquisition");
         Build_Transport; Begin_Request (Length => Packet_Bytes + 1); Trace ("");
         Check (Report.Status = Sessions.Invalid_Extent, "oversized extent refused before acquisition");
         Build_Transport; Begin_Request (Peer => Expected_Peer - 1); Trace ("SAP");
         Check (Report.Operation.Status = Loans.Acquire_Refused and then C.Evaluations = 0,
           "exact full64-bit expected owner enforced by modeled backend");
         Build_Transport; Begin_Request (Ref => (Expected_Reference.Slot, Expected_Reference.Generation - 1)); Trace ("SAP");
         Check (Report.Operation.Status = Loans.Acquire_Refused and then C.Evaluations = 0,
           "stale opaque reference generation rejected by modeled backend");
         Build_Transport; C.Save_Fails := True; Begin_Request; Trace ("S");
         Check (Report.Status = Sessions.Save_Refused and then Report.Immediate_Reply
           and then C.Evaluations = 0, "save refusal leaves loan untouched");
         Build_Transport; C.Fail := Read_Error; Begin_Request; Trace ("SAIRP");
         Check (Report.Operation.Status = Loans.Copy_In_Failed and then C.Evaluations = 0
           and then Report.Operation.Written = 0, "read failure returns acquisition without evaluating AML owner");
         Build_Transport; C.Memory := [others => 0]; Begin_Request; Trace ("SAIERP");
         Check (Report.Operation.Status = Loans.Evaluation_Refused and then Report.Operation.Written = 0
           and then (for all Byte of Output => Byte = 0), "malformed private query exposes no partial response");
         Build_Transport; C.Fail := Write_Error; Begin_Request; Trace ("SAIEORP");
         Check (Report.Operation.Status = Loans.Copy_Out_Failed and then Report.Operation.Written = 0,
           "write failure is explicit after acquisition return");
         Build_Transport; C.Answer_Fails := True; Begin_Request; Trace ("SAIEORP");
         Check (Report.Status = Sessions.Delivery_Lost and then Sessions.Ready (Session),
           "failed delivery consumes saved reply");
         Sessions.Progress (Session, C, Report); Trace ("SAIEORP");
         Check (Report.Status = Sessions.Idle and then C.Evaluations = 1 and then C.Answers = 1,
           "lost delivery never repeats evaluation or answer");
         Build_Transport; C.Return_Fails := True; Begin_Request; Trace ("SAIEOR");
         Check (Report.Status = Sessions.Deferred and then C.Active and then C.Saved
           and then Sessions.Has_Saved_Reply (Session) and then C.Answers = 0,
           "uncertain return holds saved reply and prevents delivery");
         Begin_Request; Trace ("SAIEOR");
         Check (Report.Status = Sessions.Busy and then Report.Immediate_Reply
           and then C.Evaluations = 1, "busy session cannot overwrite pending query");
         Sessions.Progress (Session, C, Report); Trace ("SAIEORR");
         Check (Report.Status = Sessions.Deferred and then C.Evaluations = 1 and then C.Answers = 0,
           "cleanup retry performs one return without replay");
         C.Return_Fails := False; Sessions.Progress (Session, C, Report); Trace ("SAIEORRRP");
         Check (Report.Status = Sessions.Answered and then Report.Operation.Status = Loans.Completed
           and then C.Evaluations = 1 and then C.Answers = 1 and then Sessions.Ready (Session),
           "successful exact cleanup releases original response once");
         Sessions.Progress (Session, C, Report); Trace ("SAIEORRRP");
         Check (Report.Status = Sessions.Idle and then C.Answers = 1, "terminal cleanup cannot answer twice");
      end Transport_Checks;
   begin
      Invoke; Ready_Reply (W.Namespace_Not_Loaded);
      Invoke (0); Check (Result.Status = B.Dispatcher.Denied and then not Result.Routed,
        "stamp checked before readiness or observation");
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Start_Snapshot, Data => [Q.Revision (Server), 1, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK and then Q.Current (Server) = Q.Receiving, "start snapshot");
      Invoke; Ready_Reply (W.Namespace_Not_Loaded);
      Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1,
        ACPI_Service.DSDT, Table, Reply);
      Check (Reply.Status = Q.OK, "import actual AML");
      Invoke; Ready_Reply (W.Namespace_Not_Loaded);
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Finish_Snapshot, Data => [Q.Revision (Server), 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK and then Q.Current (Server) = Q.Complete, "owned startup completes");
      if Ada.Command_Line.Argument (2) = "hardware" then
         Q.Handle (Server, Q.Observer,
           (Label => Q.Read_Metrics, Data => [Q.Metrics_Region_Attempt_Page, 0, 0, 0], others => <>), Reply);
         Check (Reply.Status = Q.OK and then Reply.Data (1) = 1 and then
           Reply.Data (2) = Q.Revision_Number (Q.Region_Status_Code'Enum_Rep (Q.Regions_Unavailable_Code)),
           "hardware region barrier remains unavailable");
      end if;
      Wanted_Generation := W.Generation (Q.Revision (Server));
      Invoke; Ready_Reply (W.OK);
      Check (Result.Find_Result.Returned_Nodes = 1, "method-valued HID was not invoked or matched");
      Device := Nodes (Nodes'First);
      Check (Device.Instance = Own_Instance and then Device.Revision = Wanted_Generation
        and then Device.Index > 0, "full64-bit owner identity and actual server revision");
      Invoke (0); Check (Result.Status = B.Dispatcher.Denied and then not Result.Routed,
        "completed namespace does not bypass role");
      Item.Key.Instance := Own_Instance;
      Item.Key.Revision := Wanted_Generation - 1;
      Invoke; Ready_Reply (W.Stale_Generation);
      Item.Key.Revision := Wanted_Generation;
      Item.Key.Instance := 1;
      Invoke; Ready_Reply (W.Stale_Generation);
      Item := ((Kind => W.Find_By_ID, ID => W.From_Text ("PNP0A08").Value,
        others => <>), (Present => False));
      Invoke; Ready_Reply (W.OK);
      Check (Result.Find_Result.Returned_Nodes = 1, "named bus parent found");
      Bus_Device := Nodes (Nodes'First);
      Item := ((Kind => W.Find_By_Address, Instance => Own_Instance,
        Revision => Wanted_Generation, Node => Bus_Device.Index, Address => 16#15#,
        others => <>), (Present => False));
      Invoke; Ready_Reply (W.OK);
      Check (Result.Find_Result.Returned_Nodes = 1 and then
        Nodes (Nodes'First).Instance = Own_Instance and then
        Nodes (Nodes'First).Revision = Wanted_Generation and then
        Nodes (Nodes'First).Index /= Device.Index,
        "bus ADR search uses owned scope");
      Target (W.Resources, Device); Invoke; Ready_Reply (W.OK);
      Check (Result.Resources_Result.Header.Record_Count = 1, "static IRQ resource projects through owner bridge");
      Item := ((Kind => W.Find_By_ID, ID => W.From_Text ("ACPI0003").Value,
        others => <>), (Present => False));
      Invoke; Ready_Reply (W.OK);
      Check (Result.Find_Result.Returned_Nodes = 1, "static CID matches method-HID device");
      Method_Device := Nodes (Nodes'First);
      Target (W.Describe, Method_Device);
      declare
         HID_Is_Method, UID_Is_Zero : Boolean := False;
         Turns : Natural := 0;
         Maximum_Description_Pages : constant Positive := 16;
      begin
         loop
            Turns := Turns + 1;
            Check (Turns <= Maximum_Description_Pages, "bounded complete description");
            Invoke; Ready_Reply (W.OK);
            declare Header : constant R.Header := Result.Describe_Result.Header; begin
               Check (Header.Record_Count > 0, "nonempty description page");
               for Index in R.U32 range 0 .. Header.Record_Count - 1 loop
                  declare
                     Field : constant F.Field_Result := F.Field_At
                       (Response (R.Header_Byte_Count .. R.Header_Byte_Count + Natural (Header.Payload_Bytes) - 1),
                        Header.Record_Count, Index, Node_Limit);
                  begin
                     Check (Field.Accepted, "typed describe field");
                     if Field.Value.Name = F.Hardware_ID then
                        HID_Is_Method := Field.Value.Status = W.Method_Valued;
                     elsif Field.Value.Name = F.Unique_ID then
                        UID_Is_Zero := Field.Value.Status = W.Present and then
                          Field.Value.Kind = F.Integer_Value and then Field.Value.Number = 0;
                     end if;
                  end;
               end loop;
            end;
            exit when not Result.Describe_Result.Header.Context.Continuation.Present;
            Item := Result.Describe_Result.Header.Context;
         end loop;
         Check (HID_Is_Method and then UID_Is_Zero and then Turns > 1,
           "complete description preserves method status and untouched aliased counter");
      end;
      Item := ((Kind => W.Find_By_ID, ID => W.From_Text ("PNP0C50").Value,
        others => <>), (Present => False));
      declare
         Total, Turns : Natural := 0;
      begin
         loop
            Turns := Turns + 1; Check (Turns <= Maximum_Visits + 1, "bounded cursor progress");
            Invoke (Visits => 1);
            Check (Result.Status = B.Dispatcher.Reply_Ready and then
              Result.Find_Result.Header.Status in W.OK | W.Not_Found, "paged typed reply");
            if Result.Find_Result.Header.Status = W.Not_Found then
               Check (Result.Find_Result.Returned_Nodes = 0 and then
                 not Result.Find_Result.Header.Context.Continuation.Present, "terminal empty page");
            end if;
            Check (Result.Find_Result.Scan.Visited_Nodes <= 1, "per-call work bound");
            Total := Total + Result.Find_Result.Returned_Nodes;
            exit when not Result.Find_Result.Header.Context.Continuation.Present;
            Item := Result.Find_Result.Header.Context;
         end loop;
         Check (Total = 1 and then Turns > 1, "complete pagination without skipped/repeated device");
      end;
      Check (Q.Revision (Server) = Initial + Snapshot_Mutations, "all query outcomes preserve revision even at ceiling");
      Transport_Checks;
   end Scenario;
   procedure Failed_Module is
      Length_Field_First : constant Positive := 5;
      Checksum_Position : constant Positive := 10;
      Length_Field_Bytes : constant Positive := 4;
      Byte_Base : constant Positive := 256;
      Bad_Opcode : constant Firmware_Tables.Byte := 16#FE#;
      Broken : Firmware_Tables.Bytes (1 .. Table'Length + 1);
      Remaining : Natural := Broken'Length;
      Sum : Firmware_Tables.Byte := 0;
      Server : Q.State (1, Broken'Length, Broken'Length, 1, 0);
      Reply : Q.Response;
      Frame, Response, Filter, Payload : P.Byte_Array (0 .. Packet_Bytes - 1);
      Nodes : B.Search.Node_Array (1 .. 4);
      Parents : B.Paths.Node_Array (1 .. 16);
      Reader_Data, Text_Source : Firmware_Tables.Bytes (1 .. Scratch_Bytes);
      Atoms : B.Reader.Atoms (1 .. 8);
      Result : B.Dispatcher.Report;
      Item : constant W.Request :=
        ((Kind => W.Find_By_ID, ID => W.From_Text ("PNP0C50").Value, others => <>), (Present => False));
      Accepted : Boolean;
      Written : W.Word;
      Version : Q.Revision_Number;
   begin
      Broken (1 .. Table'Length) := Table;
      Broken (Broken'Last) := Bad_Opcode;
      for I in Length_Field_First .. Length_Field_First + Length_Field_Bytes - 1 loop
         Broken (I) := Firmware_Tables.Byte (Remaining mod Byte_Base);
         Remaining := Remaining / Byte_Base;
      end loop;
      Broken (Checksum_Position) := 0;
      for Byte of Broken loop Sum := Sum + Byte; end loop;
      Broken (Checksum_Position) := 0 - Sum;
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Start_Snapshot, Data => [0, 1, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "failed-case snapshot starts");
      Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1,
        ACPI_Service.DSDT, Broken, Reply);
      Check (Reply.Status = Q.OK, "checksum-valid malformed module stages");
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Finish_Snapshot, Data => [Q.Revision (Server), 0, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.Incomplete and then Q.Current (Server) = Q.Failed,
        "actual malformed AML leaves startup failed");
      Version := Q.Revision (Server);
      P.Encode_Request (Item, [], Frame, Node_Limit, Written, Accepted);
      Check (Accepted and then Written = P.Frame_Byte_Count, "failed-case canonical query");
      Owner.Handle (Server, Own_Instance, ACPI_Protocol.Observer_Tag, Frame,
        Maximum_Visits, Resource_Item_Budget, Nodes, Parents, Reader_Data, Atoms,
        Text_Source, Filter, Payload, Response, Result);
      Check (Result.Status = B.Dispatcher.Reply_Ready and then
        Result.Find_Result.Header.Status = W.Namespace_Not_Loaded and then
        Result.Find_Result.Returned_Nodes = 0, "failed module never publishes its partial namespace");
      Check (Q.Revision (Server) = Version and then Q.Current (Server) = Q.Failed,
        "query cannot reopen or alter failed startup");
   end Failed_Module;
begin
   if Ada.Command_Line.Argument (2) = "normal" or else
     Ada.Command_Line.Argument (2) = "hardware" then Scenario (0);
   elsif Ada.Command_Line.Argument (2) = "ceiling" then Scenario (Q.Max_Revision - Snapshot_Mutations);
   elsif Ada.Command_Line.Argument (2) = "failed" then Failed_Module;
   else raise Program_Error with "unknown lifecycle case"; end if;
   Ada.Text_IO.Put_Line ("ACPI owned passive-query bridge checks" & Checks'Image);
end Query_Owner_Bridge_Tests;
