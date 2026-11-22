pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Search;
with ACPI_Passive_Query.Pages;
with ACPI_Passive_Query.Replies;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Reader is new ACPI_Passive_Members (Core => Core);
   with package Search_Owner is new ACPI_Passive_Search (Core => Core, Reader => Reader);
package ACPI_Search_Observer with SPARK_Mode is
   package Search renames Search_Owner;
   package Wire renames ACPI_Passive_Query;
   package Pages renames ACPI_Passive_Query.Pages;
   package Replies renames ACPI_Passive_Query.Replies;
   type Outcome is (Reply_Ready, Denied, Invalid_Request, Reply_Space_Required, Scratch_Required, Backend_Failed);
   type Diagnostic is (No_Diagnostic, Backend_Refusal, Internal_Scratch_Limit, Reply_Buffer_Limit);
   type Report is record
      Status : Outcome := Invalid_Request;
      Reason : Diagnostic := No_Diagnostic;
      Written, Required_Bytes, Required_Filter_Bytes : Wire.Word := 0;
      Returned_Nodes : Natural := 0;
      Backend_Invoked : Boolean := False;
      Header : Replies.Header;
      Scan : Search.Report;
   end record;
   use type Pages.Byte;
   use type Wire.Word;
   -- Stamp MUST be the kernel-stamped authorityTag, Scope the owning service's
   -- instance/revision/readiness. Neither is taken from request bytes or PID.
   -- Request_Data and every buffer are separate owned private storage. Native
   -- admission must occur before grant acquisition; this repeats that check.
   -- The native adapter still owns expected-owner loan acquisition, request
   -- copy, response copy, Return_Acquisition-before-reply and retirement.
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag; Scope : Search.Scope_State;
      Request_Data : Pages.Byte_Array; Maximum_Node_Visits : Positive;
      Nodes : out Search.Node_Array; Reader_Data : out Firmware_Tables.Bytes;
      Reader_Atoms : out Reader.Atoms; Payload, Filter, Response : out Pages.Byte_Array;
      Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Result.Status /= Reply_Ready then
         Result.Written = 0 and then (for all B of Response => B = 0))
       and then (if not ACPI_Protocol.Is_Observer (Stamp) then
         Result.Status = Denied and then not Result.Backend_Invoked);
end ACPI_Search_Observer;
