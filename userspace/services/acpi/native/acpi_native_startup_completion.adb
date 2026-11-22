pragma Ada_2022;
package body ACPI_Native_Startup_Completion is
   package D renames ACPI_Bootstrap;
   package H renames ACPI_Native_HID_Detection;
   package W renames ACPI_Native_Power_Detection;
   package C renames ACPI_Startup_Completion;
   use type D.Device_Phase;
   use type W.Outcome;
   function Classify (Devices : D.Device_Report; HID : H.Report;
      Power : W.Report; Setup_Closed : Boolean;
      Events : C.Phase_Result := C.Finished; Module_Errors : Boolean := False)
      return CuBit.Consumer_Initialization_Exchange.Phase_Status is
      Device_Result, HID_Result, Power_Result : C.Phase_Result := C.Incomplete;
   begin
      if Devices.Fatal.Present then
         Device_Result := C.Failed;
      else
         case Devices.Stage is
            when D.Devices_Not_Started | D.Devices_Failed => Device_Result := C.Failed;
            when D.Devices_Completed | D.Devices_Completed_With_Errors =>
               Device_Result := (if Devices.Errors /= 0
                 or else Devices.Stage = D.Devices_Completed_With_Errors
                 then C.Finished_With_Errors else C.Finished);
            when others => null;
         end case;
      end if;
      if HID.Cleanup_Required or else HID.Recovery_Needed then
         HID_Result := C.Failed;
      else
         case HID.Status is
            when H.Cleanup_Failed | H.Owner_Error | H.Recovery_Required => HID_Result := C.Failed;
            when H.No_Devices =>
               if HID.Candidates = 0 and then HID.Observed = 0 and then HID.Failed_Candidates = 0 then
                  HID_Result := (if HID.Node_Errors = 0 then C.Finished else C.Finished_With_Errors);
               end if;
            when H.Finished =>
               if Natural (HID.Candidates) = Natural (HID.Observed) + Natural (HID.Failed_Candidates)
                 and then HID.Node_Errors >= HID.Failed_Candidates then
                  HID_Result := (if HID.Node_Errors = 0 then C.Finished else C.Finished_With_Errors);
               end if;
            when others => null;
         end case;
      end if;
      if Power.Recovery_Needed then
         Power_Result := C.Failed;
      else
         case Power.Status is
            when W.Recovery_Required | W.Owner_Error => Power_Result := C.Failed;
            when W.No_Devices | W.Finished | W.Lid_Failed | W.Source_Failed | W.Battery_Failed =>
               -- Convert before adding: each constrained count can be 16.
               if Natural (Power.Sampled_Batteries) + Natural (Power.Absent_Batteries)
                 + Natural (Power.Failed_Batteries) = Natural (Power.Batteries) then
                  if (Power.Status = W.Battery_Failed and then Power.Failed_Batteries = 0)
                    or else (Power.Failed_Batteries /= 0 and then
                      Power.Status not in W.Battery_Failed | W.Lid_Failed | W.Source_Failed) then
                     null;
                  elsif Power.Status = W.No_Devices and then
                    (Power.Batteries /= 0 or else Power.Lid_Observed or else Power.Source_Observed
                     or else Power.Lid_Query_Failed or else Power.Source_Query_Failed) then
                     null;
                  elsif Power.Status in W.Lid_Failed | W.Source_Failed | W.Battery_Failed
                    or else Power.Lid_Query_Failed or else Power.Source_Query_Failed then
                     Power_Result := C.Finished_With_Errors;
                  else
                     Power_Result := C.Finished;
                  end if;
               end if;
            when others => null;
         end case;
      end if;
      return C.Decide (Device_Result, HID_Result, Power_Result, Setup_Closed, Events, Module_Errors);
   end Classify;
end ACPI_Native_Startup_Completion;
