pragma Ada_2022;
with GPIO_Mapping_Policy;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
package GPIO_Community_Types with SPARK_Mode, Pure is
   type Map_Outcome is (Bound, Already_Attempted, Policy_Refused, Map_Refused, Resource_Overlap, Setup_Closed, Clock_Refused, Expired);
   type Map_Report is record
      Status : Map_Outcome := Policy_Refused;
      Policy : GPIO_Mapping_Policy.Outcome := GPIO_Mapping_Policy.No_Authority;
   end record;
   type Header_Outcome is (Probed, Unbound, Already_Probed, Wrong_Controller, Geometry_Refused, Setup_Closed, Clock_Refused, Expired);
   type Header_Report is record
      Status : Header_Outcome := Unbound;
      Revision, PADBAR : Intel_GPIO_IRQ.Word := 0;
      Geometry : Intel_ADLN_GPIO.Outcome := Intel_ADLN_GPIO.No_GPIO_Pin;
   end record;
   type Pin_Admission is (Admitted, Already_Admitted, Header_Unavailable,
      Wrong_Resource, Unsupported_Pin, Pin_Geometry_Refused);
end GPIO_Community_Types;
