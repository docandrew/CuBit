pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with ACPI_Backend_Catalog;
with ACPI_Backend_Catalog_Endpoint;
generic
   with package Catalog is new ACPI_Backend_Catalog (<>);
   with procedure Poll_Completion (Found : out Boolean);
   with function Poll_Event
     (Item : out CuBit.Messages.Message; Publisher : out CuBit.Messages.Process_ID) return Boolean;
   with procedure Poll_Request
     (From : out CuBit.Messages.Process_ID; Item : out CuBit.Messages.Message; Found : out Boolean);
   with function Reply
     (To : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message) return Interfaces.Unsigned_64;
   with function Wait (Deadline : Interfaces.Unsigned_64) return CuBit.Messages.Activity_Result;
   with procedure Handle_Event
     (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message; Stop : out Boolean);
   with procedure Quarantine_Notice (Item : Catalog.Statistics);
package ACPI_Backend_Server is
   type Reason is (Not_Running, Stop_Requested, Unexpected_Completion, Wait_Unavailable, Unexpected_Deadline);
   type Counter is new Interfaces.Unsigned_64;
   type Report is record
      Status : Reason := Not_Running;
      Requests, Lost_Replies, Events : Counter := 0;
      Counter_Saturated, Quarantine_Noted : Boolean := False;
   end record;
   -- One serialized backend loop; received stamps and full process identities
   -- come from the kernel-facing adapter. Exactly one dispatch per receive,
   -- including failed/one-way replies. No callback may reenter or retry work.
   procedure Serve (Hardware : in out Catalog.Context; Result : out Report);
private
   package Endpoint is new ACPI_Backend_Catalog_Endpoint (Catalog);
end ACPI_Backend_Server;
