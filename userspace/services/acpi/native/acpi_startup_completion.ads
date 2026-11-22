pragma Ada_2022;
with CuBit.Consumer_Initialization_Exchange;
with Interfaces;
-- Classification only. Completed does not establish an I/O quiet boundary,
-- authorize a launch, or permit retirement after a possible child launch.
package ACPI_Startup_Completion with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64, CuBit.Consumer_Initialization_Exchange.Receipt;
   type Phase_Result is (Finished, Finished_With_Errors, Incomplete, Failed);
   -- Called only for unavailable EC startup. A live event domain has already
   -- established a safe boundary for independent consumers. Missing optional
   -- EC support is a completed degraded phase, not an unfinished _INI phase.
   -- This classification grants no I/O/launch authority or physical quiescence.
   function Unavailable_Events (Domain_Live, Controller_Absent, Recovery_Required : Boolean)
      return Phase_Result with Global => null,
      Post => Unavailable_Events'Result =
        (if Recovery_Required then Failed elsif not Domain_Live then Incomplete
         elsif Controller_Absent then Finished else Finished_With_Errors);
   function Decide (Devices, HID, Power : Phase_Result; Setup_Closed : Boolean;
      Events : Phase_Result := Finished; Module_Errors : Boolean := False)
      return CuBit.Consumer_Initialization_Exchange.Phase_Status
      with Global => null;
   -- This is only the completion declaration's lifecycle obligation. A
   -- validated Received receipt releasing zero launches proves no consumer was
   -- released by this declaration. Other owner/child obligations are separate.
   -- Unknown/refused receipt or positive release count after dispatch retains
   -- the owner until an explicit child-quiescence protocol exists.
   function Requires_Retention (Dispatched, Receipt_Known : Boolean;
      Answer : CuBit.Consumer_Initialization_Exchange.Receipt;
      Released : Interfaces.Unsigned_64) return Boolean
      with Global => null,
       Post => (if not Dispatched then not Requires_Retention'Result)
         and then (if Dispatched and then not Requires_Retention'Result then
           Receipt_Known and then Answer = CuBit.Consumer_Initialization_Exchange.Received and then Released = 0)
         and then (if Receipt_Known and then Answer = CuBit.Consumer_Initialization_Exchange.Received
           and then Released = 0 then not Requires_Retention'Result);
end ACPI_Startup_Completion;
