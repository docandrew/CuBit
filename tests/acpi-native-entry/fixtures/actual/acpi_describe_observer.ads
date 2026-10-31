pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Search;
with ACPI_Passive_Identity;
with ACPI_Passive_Path;
with ACPI_Passive_Query.Replies;
with ACPI_Passive_Query.Pages;
with ACPI_Passive_Query.Fields;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Reader is new ACPI_Passive_Members (Core => Core);
   with package Search_Owner is new ACPI_Passive_Search (Core => Core, Reader => Reader);
   with package Identity is new ACPI_Passive_Identity (Core => Core, Reader => Reader);
   with package Paths is new ACPI_Passive_Path (Core => Core);
package ACPI_Describe_Observer with SPARK_Mode is
   package Wire renames ACPI_Passive_Query;
   package Pages renames ACPI_Passive_Query.Pages;
   package Fields renames ACPI_Passive_Query.Fields;
   package Replies renames ACPI_Passive_Query.Replies;
   type Outcome is (Reply_Ready, Denied, Invalid_Request, Reply_Space_Required, Backend_Failed);
   type Diagnostic is (No_Diagnostic, Internal_Scratch_Limit, Backend_Refusal);
   type Report is record
      Status : Outcome := Invalid_Request;
      Reason : Diagnostic := No_Diagnostic;
      Written, Required_Bytes : Wire.Word := 0;
      Backend_Invoked : Boolean := False;
      Header : Replies.Header;
      Path_Result : Paths.Report;
      Identity_Result : Identity.Report;
   end record;
   use type Pages.Byte;
   use type Wire.Word;
   -- Authority stamp and scope are trusted owner inputs, never request words.
   -- Every array is separate private storage. The native adapter must admit
   -- before expected-owner grant acquisition, copy request/reply, return its
   -- acquisition before replying, and leave retirement to the grant owner.
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag;
      Scope : Search_Owner.Scope_State; Request_Data : Pages.Byte_Array;
      Path_Scratch : out Paths.Node_Array; Reader_Data : out Firmware_Tables.Bytes;
      Reader_Atoms : out Reader.Atoms; Text : out Firmware_Tables.Bytes;
      Payload, Response : out Pages.Byte_Array; Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Result.Status /= Reply_Ready then
         Result.Written = 0 and then (for all B of Response => B = 0))
       and then (if not ACPI_Protocol.Is_Observer (Stamp) then
         Result.Status = Denied and then not Result.Backend_Invoked);
end ACPI_Describe_Observer;
