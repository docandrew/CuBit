pragma Ada_2022;
with ACPI_EC_Registers;
with ACPI_EC_Types;
with ACPI_EC_Event_Types;
with CuBit.Process_IDs;
--  Trusted admitted event controller only, serialized with Field/event owners.
--  Publish is a bounded nonblocking enqueue, never an AML or peer call. It
--  truthfully distinguishes refusal from unknown delivery and does not reenter.
generic
   with package Registers is new ACPI_EC_Registers (<>);
   with procedure Publish
     (Item : ACPI_EC_Event_Types.Notice;
      Result : out ACPI_EC_Event_Types.Publication);
   Expected_Consumer : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
package ACPI_EC_Event_Publisher with SPARK_Mode is
   type Phase is (Unconfigured, Configuration_Refused, Ready, Querying,
      Publication_Pending, Delivery_Stalled, Delivery_Unknown,
      Query_Failed, Correlation_Failed);
   type Query_Response is (No_Query_Response, No_Pending_Event,
      Zero_Query_Response, Event_Response);
   type Report is record
      Stage : Phase := Unconfigured;
      Token : Registers.Operation_ID := 0;
      Hardware : ACPI_EC_Types.Outcome := ACPI_EC_Types.Idle;
      Hardware_Recovery : Boolean := False;
      Response : Query_Response := No_Query_Response;
      Event : ACPI_EC_Types.Byte := 0;
      Publication_Attempts, Remaining : ACPI_EC_Types.Work_Budget := 0;
   end record;
   function Observe return Report;
   -- Response/Event describe this accepted query, independently of publication.
   -- No_Query_Response includes in-flight/failed queries; inspect Stage/Hardware.
   -- No_Pending_Event is the query's status observation with SCI_EVT clear;
   -- Zero_Query_Response is a completed QR_EC returning zero. Neither attests
   -- future hardware silence. Event_Response remains after enqueue/claim.
   -- Only a newly accepted query clears it; a refused submission preserves it.
   function Retained return ACPI_EC_Event_Types.Notice_Option;
   --  One configuration attempt per instance. Identity is independently
   --  admitted controller/source routing, not a caller-provided wire claim.
   --  This API grants no interrupt, register, protocol or namespace authority.
   procedure Configure
     (Controller : ACPI_EC_Event_Types.Controller_ID;
      Epoch : ACPI_EC_Event_Types.Epoch_ID; Accepted : out Boolean);
   procedure Start_Query
     (Deadline : ACPI_EC_Types.Microseconds; Budget : ACPI_EC_Types.Work_Budget;
      Delivery_Attempts : ACPI_EC_Event_Types.Delivery_Budget;
      Result : out Registers.Submission);
   --  At most one transaction Step OR one publication attempt. Query results
   --  are retained before a later Advance enqueues them. The catalog releases
   --  normal query occupancy first, allowing subsequent AML EC Field readback.
   procedure Advance;
   --  Explicit capacity retry of exactly the retained notice, no new hardware
   --  query. Unknown delivery cannot be retried or reset through this API.
   procedure Resume_Publication
     (Attempts : ACPI_EC_Event_Types.Delivery_Budget; Accepted : out Boolean);
   --  A safely quiescent failed query may permit a fresh admitted trigger.
   --  This never repeats a handler or reconciles hardware recovery. Last
   --  hardware outcome remains observable; lost-event policy belongs outside.
   procedure Resume_Queries (Accepted : out Boolean);
   --  Serialized trusted transport entry. Sender is kernel-authenticated
   --  metadata, never a payload claim. Exact receiver claim releases only
   --  unknown publication retention, not hardware recovery or AML obligations.
   --  No_Process disables receipt acceptance until a consumer is provisioned.
   procedure Confirm_Claim
     (Sender : CuBit.Process_IDs.Process_ID;
      Item : ACPI_EC_Event_Types.Notice; Accepted : out Boolean);
end ACPI_EC_Event_Publisher;
