pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Search;
with ACPI_Passive_Identity;
with ACPI_Passive_Path;
with ACPI_Search_Observer;
with ACPI_Describe_Observer;
with ACPI_Resources_Observer;
with ACPI_Protocol;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Reader is new ACPI_Passive_Members (Core => Core);
   with package Search_Owner is new ACPI_Passive_Search (Core => Core, Reader => Reader);
   with package Identity_Owner is new ACPI_Passive_Identity (Core => Core, Reader => Reader);
   with package Path_Owner is new ACPI_Passive_Path (Core => Core);
package ACPI_Passive_Dispatch with SPARK_Mode is
   package Search renames Search_Owner;
   package Wire renames Search.Wire;
   package Pages renames Search.Pages;
   package Find is new ACPI_Search_Observer (Core, Reader, Search_Owner);
   package Describe is new ACPI_Describe_Observer (Core, Reader, Search_Owner, Identity_Owner, Path_Owner);
   package Resources is new ACPI_Resources_Observer (Core, Reader, Search_Owner);
   type Outcome is (Reply_Ready, Denied, Invalid_Request, Reply_Space_Required,
                    Scratch_Required, Backend_Failed);
   type Report is record
      Status : Outcome := Invalid_Request;
      Selected : Wire.Operation := Wire.Find_By_ID;
      Routed : Boolean := False;
      Written, Required_Bytes : Wire.Word := 0;
      Find_Result : Find.Report;
      Describe_Result : Describe.Report;
      Resources_Result : Resources.Report;
   end record;
   use type Pages.Byte;
   use type Wire.Word;
   -- One configured owner for every query kind. Stamp/scope are trusted owner
   -- inputs. Data and all scratch/output arrays are separate private storage.
   -- State may change internal passive read bookkeeping; no immutable metrics-
   -- observer model contract is claimed. Native loan lifecycle stays outside.
   procedure Handle
     (Service : in out Core.State; Stamp : ACPI_Protocol.Role_Tag;
      Scope : Search.Scope_State; Request_Data : Pages.Byte_Array;
      Maximum_Node_Visits : Positive; Maximum_Resource_Items : Resources.Data_IO.Item_Count;
      Nodes : out Search.Node_Array; Parents : out Path_Owner.Node_Array;
      Reader_Data : out Firmware_Tables.Bytes; Reader_Atoms : out Reader.Atoms;
      Text_Source : out Firmware_Tables.Bytes; Filter, Payload, Response : out Pages.Byte_Array;
      Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Result.Status /= Reply_Ready then
         Result.Written = 0 and then (for all B of Response => B = 0))
       and then (if not ACPI_Protocol.Is_Observer (Stamp) then
         Result.Status = Denied and then not Result.Routed);
end ACPI_Passive_Dispatch;
