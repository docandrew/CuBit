pragma Ada_2022;
with CuBit.Handles;
with GPIO_Mapping_Policy;
with GPIO_Community_Types;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
package GPIO_Native_Communities is
   package P renames GPIO_Mapping_Policy;
   package T renames GPIO_Community_Types;
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   type Resource_Count is range 0 .. Integer (G.Memory_Resource_Ordinal'Last) + 1;
   procedure Bind
     (Resource : G.Memory_Resource_Ordinal; Authority : CuBit.Handles.Device_Memory;
      Physical, Bytes, Virtual, Page_Limit : P.Address; Deadline : I.Tick;
      Result : out T.Map_Report);
   procedure Probe
     (Resource : G.Memory_Resource_Ordinal; Controller_HID : String;
      Deadline : I.Tick; Result : out T.Header_Report);
   procedure Freeze (Ready_Resources : out Resource_Count);
   function Ready (Resource : G.Memory_Resource_Ordinal) return Boolean;
   procedure Admit_Pin (Pin : G.ACPI_Pin; Result : out T.Pin_Admission; Plan : out G.Plan_Result);
   procedure Read
     (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status);
   procedure Write
     (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status);
   -- Four ordinal-selected admitted resources, never hard-coded physical bases.
   -- Setup is one-attempt/resource. Freeze prevents later map/probe changes;
   -- runtime pin admission/access require a successful frozen resource.
   -- Clock bounds surround header probe. Kernel map success and exact cached
   -- canonical plans remain required; geometry alone is not a pin capability.
end GPIO_Native_Communities;
