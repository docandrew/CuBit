pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Search;
with ACPI_Passive_Resource_Projection;
with ACPI_Passive_Query.Pages;
with ACPI_Passive_Query.Replies;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Reader is new ACPI_Passive_Members (Core => Core);
   with package Search_Owner is new ACPI_Passive_Search (Core => Core, Reader => Reader);
package ACPI_Resources_Observer with SPARK_Mode is
   package Wire renames ACPI_Passive_Query;
   package Pages renames ACPI_Passive_Query.Pages;
   package Replies renames ACPI_Passive_Query.Replies;
   package Projection renames ACPI_Passive_Resource_Projection;
   package Records renames Projection.Records;
   package Data_IO renames Projection.Data_IO;
   type Outcome is (Reply_Ready, Denied, Invalid_Request, Reply_Space_Required, Backend_Failed);
   type Diagnostic is (No_Diagnostic, Internal_Scratch_Limit, Template_Quota,
                       Invalid_Template, Invalid_Descriptor, Backend_Refusal);
   type Report is record
      Status : Outcome := Invalid_Request;
      Reason : Diagnostic := No_Diagnostic;
      Written, Required_Bytes, Required_Source_Bytes : Wire.Word := 0;
      Backend_Invoked : Boolean := False;
      Header : Replies.Header;
      Observation : Reader.Report;
      Scan : Data_IO.Scan_Result;
      Projected : Projection.Report;
      Checked_Descriptors : Data_IO.Item_Count := 0;
      Failed_Offset : Data_IO.Byte_Offset := 0;
   end record;
   use type Pages.Byte;
   use type Wire.Word;
   -- Trusted kernel stamp/owner scope, serialized namespace, and separate
   -- private buffers are mandatory. Native loan admission/acquire/copy/return
   -- and retirement are outside this adapter. Facts confer no hardware rights.
   -- Full template framing/checksum/quota and all descriptor forms are checked
   -- before emitting one selected subentry. Cursor offsets must be reached by
   -- walking genuine frames from zero, never by parsing arbitrary client bytes.
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag;
      Scope : Search_Owner.Scope_State; Request_Data : Pages.Byte_Array;
      Maximum_Items : Data_IO.Item_Count; Template : out Firmware_Tables.Bytes;
      Reader_Atoms : out Reader.Atoms; Source : out Firmware_Tables.Bytes;
      Payload, Response : out Pages.Byte_Array; Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Result.Status /= Reply_Ready then
         Result.Written = 0 and then (for all B of Response => B = 0))
       and then (if not ACPI_Protocol.Is_Observer (Stamp) then
         Result.Status = Denied and then not Result.Backend_Invoked);
end ACPI_Resources_Observer;
