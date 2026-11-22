-- Semantic-only actual runtime instantiation: never link or execute.
pragma Ada_2022;
with ACPI_EC_Types; with ACPI_EC_Event_Types;
with ACPI_EC_Backend_Owner; with ACPI_EC_Backend_Owner.With_Receive;
with ACPI_EC_Native_Clock; with CuBit.Messages; with CuBit.Process_IDs;
procedure Native_Semantic_Tests is
   procedure Read (Register : ACPI_EC_Types.Read_Register;
      Value : out ACPI_EC_Types.Byte; Result : out ACPI_EC_Types.Access_Result) is
      pragma Unreferenced (Register);
   begin Value := 0; Result := ACPI_EC_Types.Refused; end Read;
   procedure Write (Register : ACPI_EC_Types.Write_Register;
      Value : ACPI_EC_Types.Byte; Result : out ACPI_EC_Types.Access_Result) is
      pragma Unreferenced (Register, Value);
   begin Result := ACPI_EC_Types.Refused; end Write;
   function Owned return Boolean is (False);
   procedure Publish (Item : ACPI_EC_Event_Types.Notice;
      Result : out ACPI_EC_Event_Types.Publication) is
      pragma Unreferenced (Item);
   begin Result := ACPI_EC_Event_Types.Backpressure; end Publish;
   Sender : constant CuBit.Process_IDs.Process_ID :=
      CuBit.Process_IDs.From_Word (16#ABCD_1234_0000_0001#);
   Consumer : constant CuBit.Process_IDs.Process_ID :=
      CuBit.Process_IDs.From_Word (16#DCBA_4321_0000_0002#);
   package Owner is new ACPI_EC_Backend_Owner
     (Read, Write, Owned, Publish, 100, 100, 2,
      Expected_Consumer => Consumer, Clock => ACPI_EC_Native_Clock.Read,
      Pause => ACPI_EC_Native_Clock.Pause_Until);
   package Receiver is new Owner.With_Receive;
   Cells : constant Owner.Cell_List := [1 => (Register => 17, Epoch => 1,
      Owner => Sender, Cell => 16#10#, Readable => True, others => <>)];
   Setup : Owner.Setup_Result;
   Call : Owner.Call_Report; Query : Owner.Query_Status; Work : Owner.Progress_Report;
   Turn : Receiver.Report; Drain : Receiver.Drain_Report;
   Revoke_Accepted, Install_Accepted, Claim_Accepted, Publication_Accepted, Query_Accepted : Boolean;
   Scan_Request : Owner.Query_Request_Result; Scan_Accepted : Boolean;
begin
   Owner.Configure (Cells, 42, 1, Setup);
   Owner.Request_Query_Scan (1, 2, Scan_Request); Owner.Resume_Query_Scan (2, Scan_Accepted);
   Owner.Begin_Call (Sender, CuBit.Messages.NULL_MESSAGE, Call);
   Owner.Start_Query (1, Query); Owner.Advance (Work);
   Receiver.Turn (Turn); Receiver.Drain (Drain);
   Owner.Revoke (17, 1, Revoke_Accepted); Owner.Install (Cells (1), Install_Accepted);
   Owner.Confirm_Claim (Consumer, (42, 1, 1, 1), Claim_Accepted);
   Owner.Resume_Publication (1, Publication_Accepted); Owner.Resume_Queries (Query_Accepted);
end Native_Semantic_Tests;
