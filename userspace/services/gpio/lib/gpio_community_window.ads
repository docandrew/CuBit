pragma Ada_2022;
with GPIO_Mapping_Policy;
with GPIO_Community_Types;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
generic
   type Authority_Type is private;
   with procedure Inspect
     (Authority : Authority_Type; Result : out GPIO_Mapping_Policy.Description);
   with procedure Map
     (Authority : Authority_Type; Physical, Virtual, Pages : GPIO_Mapping_Policy.Address;
      Mapped : out Boolean);
package GPIO_Community_Window with SPARK_Mode => Off is
   package P renames GPIO_Mapping_Policy;
   package T renames GPIO_Community_Types;
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   procedure Bind
     (Authority : Authority_Type; Resource : G.Memory_Resource_Ordinal;
      Physical, Bytes, Virtual, Page_Limit : P.Address; Result : out T.Map_Report);
   procedure Probe (Controller_HID : String; Result : out T.Header_Report);
   function Ready return Boolean;
   procedure Admit_Pin (Pin : G.ACPI_Pin; Result : out T.Pin_Admission; Plan : out G.Plan_Result);
   procedure Read
     (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status);
   procedure Write
     (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status);
   -- One mapping and header probe/resource. Admitted canonical pin plans are
   -- cached independently; each callback requires an EXACT cached-plan match.
   -- No caller-selected raw offset. Admit_Pin creates internal geometry only,
   -- not client pin authority. Kernel capability lifetime/exclusivity and
   -- controller-wide RMW serialization remain trusted native obligations.
end GPIO_Community_Window;
