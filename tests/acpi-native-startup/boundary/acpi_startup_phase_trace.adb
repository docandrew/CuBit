pragma Ada_2022;
package body ACPI_Startup_Phase_Trace with SPARK_Mode is
   function Name (Value : Phase_Code) return String is
     (case Value is
        when No_Phase => "NO_PHASE",
        when Early_Node_Errors => "EARLY_NODE_ERRORS",
        when HID_Discovery => "HID_DISCOVERY",
        when HID_Path_Diagnostics => "HID_PATH_DIAGNOSTICS",
        when HID_Module_Notes => "HID_MODULE_NOTES",
        when Power_Discovery => "POWER_DISCOVERY",
        when Power_Module_Notes => "POWER_MODULE_NOTES",
        when Close_Startup => "CLOSE_STARTUP",
        when Enter_Runtime => "ENTER_RUNTIME",
        when Register_Service => "REGISTER_SERVICE",
        when Arm_SCI => "ARM_SCI",
        when Start_Events => "START_EVENTS",
        when Offer_HID => "OFFER_HID",
        when Offer_Path_Diagnostics => "OFFER_PATH_DIAGNOSTICS",
        when Complete_Consumers => "COMPLETE_CONSUMERS",
        when Serve => "SERVE");
   function Name (Value : Phase_Edge) return String is
     (case Value is
        when Not_Started => "NOT_STARTED",
        when Phase_Entered => "PHASE_ENTERED",
        when Phase_Exited => "PHASE_EXITED");
   function Capture (Value : String) return Outcome is
      Result : Outcome;
   begin
      Result.Length := Natural'Min (Value'Length, Maximum_Outcome_Characters);
      Result.Truncated := Value'Length > Maximum_Outcome_Characters;
      for I in 1 .. Result.Length loop
         Result.Data (I) := Value (Value'First + (I - 1));
      end loop;
      return Result;
   end Capture;
   function Text (Value : Outcome) return String is
     (Value.Data (1 .. Value.Length));
   False_Label : constant String := "FALSE";
   True_Label : constant String := "TRUE";
   function Flag_Image (Value : Boolean) return String
     with Global => null,
       Post => Flag_Image'Result'Length <= False_Label'Length
   is
   begin
      return (if Value then True_Label else False_Label);
   end Flag_Image;
   function Image (Value : Checkpoint; Publisher_Dropped : Interfaces.Unsigned_64)
      return String is
     ("acpi: phase-checkpoint last=" & Name (Value.Last_Phase) &
      " edge=" & Name (Value.Edge) & " result=" & Text (Value.Result) &
      " truncated=" & Flag_Image (Value.Result.Truncated) &
      " consistent=" & Flag_Image (Value.Consistent) &
      " non-success=" & Flag_Image (Value.Failure_Present) &
      " failed-phase=" & Name (Value.Failed_Phase) &
      " failed-result=" & Text (Value.Failed_Result) &
      " failed-truncated=" & Flag_Image (Value.Failed_Result.Truncated) &
      " publisher-dropped=" & Interfaces.Unsigned_64'Image (Publisher_Dropped));
   procedure Enter (Item : in out State; Phase : Measured_Phase) is
   begin
      if Item.Entered (Phase) then Item.View.Consistent := False; end if;
      Item.Entered (Phase) := True;
      Item.View.Last_Phase := Phase;
      Item.View.Edge := Phase_Entered;
      Item.View.Result := Capture ("ENTERED");
      Item.View.Succeeded := False;
   end Enter;
   procedure Leave (Item : in out State; Phase : Measured_Phase;
      Result : String; Succeeded : Boolean) is
   begin
      if not Item.Entered (Phase) or else Item.Exited (Phase) then
         Item.View.Consistent := False;
      end if;
      Item.Exited (Phase) := True;
      Item.View.Last_Phase := Phase;
      Item.View.Edge := Phase_Exited;
      Item.View.Result := Capture (Result);
      Item.View.Succeeded := Succeeded;
      if not Succeeded then
         Item.View.Failure_Present := True;
         Item.View.Failed_Phase := Phase;
         Item.View.Failed_Result := Item.View.Result;
      end if;
   end Leave;
   function Observe (Item : State) return Checkpoint is (Item.View);
end ACPI_Startup_Phase_Trace;
