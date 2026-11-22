pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Service_Core.Runtime_Status;
with ACPI_Passive_Members;
with ACPI_Passive_Search;
with ACPI_Passive_Identity;
with ACPI_Passive_Path;
with ACPI_Passive_Dispatch;
-- Service-owner child: no Core access or caller-supplied readiness escapes.
package ACPI_Bootstrap.Passive_Queries with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Runtime_Status is new ACPI_Service.Runtime_Status;
   package Reader is new ACPI_Passive_Members (Core => ACPI_Service);
   package Search is new ACPI_Passive_Search (Core => ACPI_Service, Reader => Reader);
   package Identity is new ACPI_Passive_Identity (Core => ACPI_Service, Reader => Reader);
   package Paths is new ACPI_Passive_Path (Core => ACPI_Service);
   package Dispatcher is new ACPI_Passive_Dispatch
     (Core => ACPI_Service, Reader => Reader, Search_Owner => Search,
      Identity_Owner => Identity, Path_Owner => Paths);
   package Wire renames Search.Wire;
   package Pages renames Search.Pages;
   use type Dispatcher.Outcome;
   use type Pages.Byte;
   use type Wire.Word;
   -- Owner_Instance and Snapshot_Generation are trusted process-lifetime/revision facts,
   -- never request claims. Static discovery does not require device _INI or
   -- region readiness: it invokes no AML/Field and confers no resource rights.
   -- Catalog completion and member initialization ARE required. All arrays
   -- are separate stable private storage; native admission/loans stay outside.
   procedure Handle
     (Session : in out State; Owner_Instance : Wire.Instance_ID;
      Snapshot_Generation : Wire.Generation;
      Stamp : ACPI_Protocol.Role_Tag; Request_Data : Pages.Byte_Array;
      Maximum_Node_Visits : Positive;
      Maximum_Resource_Items : Dispatcher.Resources.Data_IO.Item_Count;
      Nodes : out Search.Node_Array; Parents : out Paths.Node_Array;
      Reader_Data : out Firmware_Tables.Bytes; Reader_Atoms : out Reader.Atoms;
      Text_Source : out Firmware_Tables.Bytes;
      Filter, Payload, Response : out Pages.Byte_Array;
      Result : out Dispatcher.Report)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Current (Session) = Current (Session)'Old
       and then Installed (Session) = Installed (Session)'Old
       and then Catalog_Model (Session) = Catalog_Model (Session)'Old
       and then (if Result.Status /= Dispatcher.Reply_Ready then
         Result.Written = 0 and then (for all B of Response => B = 0));
end ACPI_Bootstrap.Passive_Queries;
