pragma Ada_2022;
with GPIO_Service_Loop;
with GPIO_Controller_Resources;
with GPIO_Channel_Badges;
with GPIO_Community_Types;
with Intel_ADLN_GPIO;
generic
   with package Registry is new GPIO_Service_Loop (<>);
   with procedure Admit_Pin (Pin : Intel_ADLN_GPIO.ACPI_Pin;
      Result : out GPIO_Community_Types.Pin_Admission; Plan : out Intel_ADLN_GPIO.Plan_Result);
package GPIO_Startup with SPARK_Mode is
   package C renames Registry.C;
   package I renames C.I;
   package B renames GPIO_Channel_Badges;
   package G renames Intel_ADLN_GPIO;
   type Inventory is array (Positive range <>) of B.Interrupt_Input;
   type Phase is (Cold, Registering, Await_Registration, Parking, Ready, Failed);
   type Failure is (None, Not_Empty, Empty_Inventory, Capacity_Refused, Invalid_Input,
      Unavailable_Pin, Duplicate_Pin, Registration_Refused, Controller_Faulted,
      Layout_Changed, Unexpected_Pin_State, Seal_Refused, Parking_Refused);
   type Report is record
      At_Phase : Phase := Cold;
      Reason : Failure := None;
      Count, Registered : GPIO_Controller_Resources.Pin_Count := 0;
      Last_Controller : C.Admission := C.Accepted;
      Last_Admission : GPIO_Community_Types.Pin_Admission := GPIO_Community_Types.Header_Unavailable;
   end record;
   function Observe return Report;
   procedure Start (Pins : Inventory; Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean);
   procedure Step;
   -- One startup owner per registry. Trusted launcher inventory only, after
   -- named community capabilities are mapped/probed/frozen and while parent IRQ
   -- is disabled. Geometry is not authority. Preflight validates every entry and
   -- duplicate before controller MMIO. Register_Parked then initializes one pin
   -- at a time, never enabling it. The registry is sealed only after all pins
   -- finish masked. Ready requires operational, quiescent sealed registry.
   -- Step performs at most one controller Advance; no waits/retries/time extension.
   -- Any fault is terminal and retains diagnostics/resources for supervision.
end GPIO_Startup;
