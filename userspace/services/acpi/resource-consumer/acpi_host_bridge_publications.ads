pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Host_Bridge_Query;
with ACPI_Host_Bridge_Resources;
with ACPI_Host_Bridge_Channel;
with ACPI_Host_Bridge_Projection;
with Interfaces;
with CuBit.Handles;
with CuBit.Protocols;
with CuBit.Messages;
with CuBit.Control_Events;
package ACPI_Host_Bridge_Publications is
   package B renames ACPI_Host_Bridge_Channel;
   package W renames B.W;
   package S renames B.S;
   package H renames ACPI_Host_Bridge_Resources;
   package M renames B.M;
   type Failure_Kind is (Missing_Resources, Evaluation_Failed, Wrong_Result_Type,
      Budget_Exhausted, Invalid_Resources, Descriptor_Quota, Unsupported_Descriptor,
      Invalid_Descriptor, Descriptor_Capacity, Projection_Failed, Coordinate_Range,
      Transport_Capacity);
   type Reason_Codes is array (Failure_Kind) of M.Failure_Reason;
   Canonical_Reasons : constant Reason_Codes :=
     [Missing_Resources => M.Missing_Resources, Evaluation_Failed => M.Evaluation_Failed,
      Wrong_Result_Type => M.Wrong_Result_Type, Budget_Exhausted => M.Quota,
      Invalid_Resources => M.Malformed_Resource, Descriptor_Quota => M.Quota,
      Unsupported_Descriptor => M.Unknown_Descriptor, Invalid_Descriptor => M.Malformed_Resource,
      Descriptor_Capacity => M.Capacity, Projection_Failed => M.Malformed_Resource,
      Coordinate_Range => M.Malformed_Coordinates, Transport_Capacity => M.Capacity];
   generic
      with package Core is new ACPI_Service_Core (<>);
      with procedure Evaluate_Named
        (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
         Budget : Natural; Result : out Core.Values.Result;
         Status : out Core.Values.Access_Status) is Core.Evaluate_Named;
      -- Native owner closes the SAME original AML access window. It must
      -- preserve Core/catalog identity and copied buffers, never evaluate AML
      -- or do peer IPC. Healthy=False must not establish a quiet boundary.
      with procedure End_Query_Window
        (Healthy : Boolean; Closed : out Boolean; Now_Milliseconds : out W.Word);
      Wire_Reasons : Reason_Codes := Canonical_Reasons;
   package Producers is
      package Q is new ACPI_Host_Bridge_Query (Core, Evaluate_Named);
      type Phase is (Not_Started, Awaiting_Buffer, Ready_For_Report, No_Report, Opening,
         Setup_Refused, Transport_Refused, Window_Refused, Recovery_Required);
      type Report is record
         Stage : Phase := Not_Started;
         Started, Working, Window_End_Attempted, Window_Closed : Boolean := False;
         Has_Snapshot, Recovery_Needed : Boolean := False;
         Query : Q.Report;
         Collection : H.Collection_Report;
         Projection : ACPI_Host_Bridge_Projection.Outcome := ACPI_Host_Bridge_Projection.Invalid_Descriptor;
         Header : S.Header;
         Source_Bytes : Natural := 0;
      end record;
      type Session is limited private;
      function Observe (Run : Session) return Report;
      function Transport (Run : Session) return B.Report;
      -- Caller opens the serialized original-deadline AML access window first.
      -- A session consumes one selected Device and never reevaluates on copy,
      -- capacity, transport, timeout or peer failures. IDs are caller-assigned
      -- opaque identities scoped to this authenticated producer/catalog session.
      procedure Start (Run : in out Session; Service : aliased in out Core.State;
         Device : Core.Node_Descriptor; Root_ID, Sequence : W.Word;
         Budget : Natural; Maximum_Items : H.R.Item_Count);
      -- Caller-selected Data storage may grow while the SAME query is retained.
      -- Descriptors/Rows are scratch only, not retained or exported by reference.
      -- The successful source is encoded/copied into the transport before this
      -- call returns. No channel submission precedes confirmed window closure.
      -- Failure/incompleteness sends only a header, never partial rows/bytes.
      procedure Copy_And_Open (Run : in out Session; Service : aliased in out Core.State;
         Data : out S.Bytes; Descriptors : out H.Descriptor_List; Rows : out W.Rows;
         Endpoint : CuBit.Handles.Endpoint; Peer : B.P.Process_ID;
         Schema : CuBit.Protocols.Schema_Contract; Connector : Interfaces.Unsigned_16;
         Open_Token, Data_Token, Deadline : W.Word; Submitted : out Boolean);
      -- Only a still-pending query is cancelled; failed releases are not retried.
      procedure Cancel (Run : in out Session; Service : aliased in out Core.State);
      procedure Complete_Open (Run : in out Session; Item : CuBit.Messages.CompletionEntry; Handled : out Boolean);
      procedure Advance (Run : in out Session; Now : W.Word; Submitted : out Boolean);
      procedure Prepare_Wait (Run : in out Session; Armed : out Boolean);
      procedure Disarm (Run : in out Session);
      procedure Note_Event (Run : in out Session; Event : CuBit.Control_Events.Event; Ends : out Boolean);
      procedure Close (Run : in out Session; Completed : out Boolean);
      function Retired (Run : Session) return Boolean;
   private
      type Session is limited record
         Info : Report;
         Query : Q.Session;
         Channel : B.Session;
      end record;
   end Producers;
end ACPI_Host_Bridge_Publications;
