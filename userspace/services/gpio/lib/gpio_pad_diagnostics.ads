pragma Ada_2022;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
-- Behavioural diagnostic targets informed by Linux v6.12 pinctrl-intel:
-- HOSTSW_OWN distinguishes GPIO/ACPI mode; pad routing and GPI_IS/GPI_IE
-- distinguish configuration from interrupt delivery. No Linux code used.
generic
   with procedure Read_Register
     (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Read_ID;
      Value : out Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status);
package GPIO_Pad_Diagnostics with SPARK_Mode is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   type Register_Kind is (Host_Owner, Pad_Configuration, Group_Status, Group_Enable);
   type Reading is record
      Value : I.Word := 0;
      Status : I.Access_Status := I.Unavailable;
      Attempted : Boolean := False;
   end record;
   type Snapshot is array (Register_Kind) of Reading;
   procedure Capture (Plan : G.Plan_Result; Idle : Boolean; Result : out Snapshot);
   -- Caller owns the admitted plan and serial event loop. Only a quiescent,
   -- nonfaulted controller may pass Idle. Four bounded read-only accesses;
   -- stop on first refusal, no retry/write/lease manipulation or logging.
end GPIO_Pad_Diagnostics;
