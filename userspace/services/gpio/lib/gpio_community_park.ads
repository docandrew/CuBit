pragma Ada_2022;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with GPIO_Community_Types;
generic
   with function Resource_Ready (Resource : Intel_ADLN_GPIO.Memory_Resource_Ordinal) return Boolean;
   with procedure Admit_Pin (Pin : Intel_ADLN_GPIO.ACPI_Pin;
      Result : out GPIO_Community_Types.Pin_Admission; Plan : out Intel_ADLN_GPIO.Plan_Result);
   with procedure Clock (Now : out Intel_GPIO_IRQ.Tick; Available : out Boolean);
   with procedure Read (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Read_ID;
      Value : out Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status);
   with procedure Write (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Intel_GPIO_IRQ.Write_ID;
      Value : Intel_GPIO_IRQ.Word; Status : out Intel_GPIO_IRQ.Access_Status);
package GPIO_Community_Park with SPARK_Mode is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   type Work_Count is range 0 .. 65_535;
   subtype Work_Limit is Work_Count range 1 .. Work_Count'Last;
   type Phase is (Cold, Select_Pin, Read_Owner, Read_Host, Read_Enable,
      Mask_Write, Mask_Confirm, Clear_Status, Clear_Confirm, Ready, Failed, Cancelled);
   type Failure is (None, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Budget_Exhausted, Geometry_Refused, Register_Refused, Register_Indeterminate,
      Mask_Not_Confirmed, Stop_Requested, No_Resources);
   type Report is record
      At_Phase, Last_Access_Phase : Phase := Cold;
      Reason : Failure := None;
      Visited, Parked, Firmware_Owned, Unmapped : Work_Count := 0;
      Accesses, Completed_Writes : Work_Count := 0;
      Starting_Budget, Consumed_Budget : Work_Count := 0;
      Last_Pin : G.ACPI_Pin := 0;
      Last_Status : I.Access_Status := I.Access_OK;
      Last_Value : I.Word := 0;
      Effects_Possible : Boolean := False;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   procedure Start (S : in out State; Deadline : I.Tick; Budget : Work_Limit; Accepted : out Boolean);
   procedure Step (S : in out State);
   procedure Request_Stop (S : in out State);
   -- Startup-only exclusive controller owner, with parent IRQ disabled.
   -- Ready requires every mapped profile GPIO to be masked/cleared or skipped
   -- after observed PAD_OWN/HOSTSW_OWN checks. Configuration is never changed;
   -- W1C writes contain only the selected host-owned GPIO bit. Status may relatch.
   -- Step performs at most one register access; no waits, deadline extension,
   -- retry, pin/mode defaults or client admission. Unknown effects stay Failed.
   -- Cancelled is partial work, never Ready or proof of whole-controller quiet.
   -- Mappings/geometry frozen; ownership stable during each mask transaction;
   -- no concurrent enable-register writers. Callbacks bounded/nonraising.
private
   function Valid_State (S : State) return Boolean with Ghost;
   type State is limited record
      Info : Report;
      Plan : G.Plan_Result;
      Cursor : G.ACPI_Pin := 0;
      Owner_Value, Desired : I.Word := 0;
      Until_Time, Previous_Time : I.Tick := 0;
      Have_Time, Stopping : Boolean := False;
      Remaining : Work_Count := 0;
   end record with Type_Invariant => Valid_State (State);
   function Active (Item : Phase) return Boolean is
     (Item in Read_Owner | Read_Host | Read_Enable | Mask_Write | Mask_Confirm | Clear_Status | Clear_Confirm);
   function Valid_State (S : State) return Boolean is
     (S.Info.Consumed_Budget <= S.Info.Starting_Budget
      and then S.Remaining = S.Info.Starting_Budget - S.Info.Consumed_Budget
      and then S.Info.Visited <= S.Info.Consumed_Budget
      and then S.Info.Accesses <= S.Info.Consumed_Budget
      and then S.Info.Completed_Writes <= S.Info.Accesses
      and then S.Info.Parked <= S.Info.Visited
      and then S.Info.Firmware_Owned <= S.Info.Visited - S.Info.Parked
      and then S.Info.Unmapped <= S.Info.Visited - S.Info.Parked - S.Info.Firmware_Owned
      and then (if S.Info.At_Phase = Cold then
         S.Info.Starting_Budget = 0 and then S.Info.Consumed_Budget = 0
         and then S.Info.Visited = 0 and then S.Info.Accesses = 0)
      and then (if Active (S.Info.At_Phase) then
         (case S.Plan.Status is when G.Ready => True, when others => False)
         and then S.Info.Unmapped < S.Info.Visited - S.Info.Parked - S.Info.Firmware_Owned
         and then S.Info.Unmapped = S.Info.Visited - S.Info.Parked - S.Info.Firmware_Owned - 1
       elsif S.Info.At_Phase in Cold | Select_Pin | Ready then
         S.Info.Unmapped = S.Info.Visited - S.Info.Parked - S.Info.Firmware_Owned
       else S.Info.Visited - S.Info.Parked - S.Info.Firmware_Owned - S.Info.Unmapped <= 1));
end GPIO_Community_Park;
