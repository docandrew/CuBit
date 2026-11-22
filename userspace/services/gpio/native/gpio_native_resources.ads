pragma Ada_2022;
with CuBit.Handles;
with GPIO_Native_Communities;
with GPIO_Mapping_Policy;
with GPIO_Community_Types;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
package GPIO_Native_Resources is
   package P renames GPIO_Mapping_Policy;
   package T renames GPIO_Community_Types;
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   type Authorities is array (G.Memory_Resource_Ordinal) of CuBit.Handles.Device_Memory;
   type Outcome is (Absent, Invalid_Authority, Invalid_Window, Mapping_Refused, Probe_Refused, Ready);
   type Resource_Report is record
      Status : Outcome := Absent;
      Description : P.Description;
      Virtual : P.Address := 0;
      Mapping : T.Map_Report;
      Header : T.Header_Report;
   end record;
   type Reports is array (G.Memory_Resource_Ordinal) of Resource_Report;
   type Report is record
      Repeated : Boolean := False;
      Ready_Count : GPIO_Native_Communities.Resource_Count := 0;
      Resources : Reports;
   end record;
   procedure Setup (Caps : Authorities; Virtual_First, Virtual_Limit, Page_Limit : P.Address;
      Deadline : I.Tick; Result : out Report);
   -- Exactly one setup attempt; maps whole admitted resources and freezes the
   -- communities. Physical base/span come ONLY from typed Describe, not ACPI
   -- payloads/parameters. Caller reserves [Virtual_First,Virtual_Limit) in its
   -- own address space. No fixed window stride, rounding or partial-page access.
   -- Missing/refused resources stay unavailable. Explicit page budget/deadline;
   -- no retries. Main admits only pins whose frozen community is Ready.
end GPIO_Native_Resources;
