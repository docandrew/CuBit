pragma Ada_2022;
package body ACPI_Startup_Completion with SPARK_Mode is
   package P renames CuBit.Consumer_Initialization_Exchange;
   function Decide (Devices, HID, Power : Phase_Result; Setup_Closed : Boolean;
      Events : Phase_Result := Finished; Module_Errors : Boolean := False)
      return P.Phase_Status is
   begin
      if Devices = Failed or else HID = Failed or else Power = Failed or else Events = Failed then
         return P.Failed;
      elsif not Setup_Closed or else Devices = Incomplete
        or else HID = Incomplete or else Power = Incomplete or else Events = Incomplete then
         return P.Incomplete;
      elsif Devices = Finished_With_Errors or else HID = Finished_With_Errors
        or else Power = Finished_With_Errors or else Events = Finished_With_Errors or else Module_Errors then
         return P.Completed_With_Errors;
      else
         return P.Completed;
      end if;
   end Decide;
   function Requires_Retention (Dispatched, Receipt_Known : Boolean;
      Answer : P.Receipt; Released : Interfaces.Unsigned_64) return Boolean is
     (Dispatched and then (not Receipt_Known or else Answer /= P.Received or else Released /= 0));
end ACPI_Startup_Completion;
