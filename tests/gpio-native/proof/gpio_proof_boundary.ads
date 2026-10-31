pragma Ada_2022;
with Intel_GPIO_IRQ;
with Intel_ADLN_GPIO;
with GPIO_Community_Types;
-- Proof-only abstract external device. Outputs/statuses are unconstrained.
-- These imported interface contracts are trusted boundaries, not proved HW.
package GPIO_Proof_Boundary with SPARK_Mode,
  Abstract_State => ((Device with External => Async_Writers),
                     (Frozen_Mappings with External => Async_Readers)) is
   procedure Clock (Now : out Intel_GPIO_IRQ.Tick; Available : out Boolean)
     with Import, Global => (In_Out => Device), Always_Terminates;
   procedure Read (ID : Intel_GPIO_IRQ.Read_ID; Value : out Intel_GPIO_IRQ.Word;
      Status : out Intel_GPIO_IRQ.Access_Status)
     with Import, Global => (In_Out => Device), Always_Terminates;
   procedure Write (ID : Intel_GPIO_IRQ.Write_ID; Value : Intel_GPIO_IRQ.Word;
      Status : out Intel_GPIO_IRQ.Access_Status)
     with Import, Global => (In_Out => Device), Always_Terminates;
   function Resource_Ready (Resource : Intel_ADLN_GPIO.Memory_Resource_Ordinal) return Boolean
     with Import, Global => (Input => Frozen_Mappings);
   procedure Admit_Pin (Pin : Intel_ADLN_GPIO.ACPI_Pin;
      Result : out GPIO_Community_Types.Pin_Admission; Plan : out Intel_ADLN_GPIO.Plan_Result)
     with Import, Global => (In_Out => Device), Always_Terminates;
   procedure Read_Plan (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Read_ID;
      Value : out Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status)
     with Import, Global => (In_Out => Device), Always_Terminates;
   procedure Write_Plan (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Write_ID;
      Value : Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status)
     with Import, Global => (In_Out => Device), Always_Terminates;
end GPIO_Proof_Boundary;
