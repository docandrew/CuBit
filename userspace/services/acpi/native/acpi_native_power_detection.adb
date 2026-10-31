pragma Ada_2022;
with ACPI_Service;
with ACPI_Lid;
with ACPI_Power_Source;
with ACPI_Requests.Lids;
with ACPI_Requests.Power_Sources;
with ACPI_Battery;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
with ACPI_Battery_Observation;
with ACPI_Requests.Battery_Observations;
with ACPI_Native_Log;
with ACPI_Requests.Sleep_Registers;
with AML_Execute;
package body ACPI_Native_Power_Detection is
   package Core renames ACPI_Service;
   package Lid_Driver is new ACPI_Lid (Core);
   package Source_Driver is new ACPI_Power_Source (Core);
   package Lids is new ACPI_Requests.Lids (Lid_Driver);
   package Sources is new ACPI_Requests.Power_Sources (Source_Driver);
   package Format is new ACPI_Battery_Information;
   package Dynamic is new ACPI_Battery (Core);
   package Static_Info is new ACPI_Battery_Information_Reader (Core, Format);
   package Collector is new ACPI_Battery_Observation (Core, Format, Dynamic, Static_Info);
   package Batteries is new ACPI_Requests.Battery_Observations (Dynamic, Collector);
   use type Lids.Controller.Sampling_Outcome, Sources.Controller.Sampling_Outcome;
   use type Batteries.Controller.Discovery_Outcome, Collector.Outcome;
   Attempted : Boolean := False;
   Saved : Report;
   function Uncertain (Execution : AML_Execute.Execution_Status) return Boolean is
     (Execution in AML_Execute.Field_Indeterminate | AML_Execute.Fatal_Requested);
   procedure Run (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>)) is
      LR : Lids.Controller.Sampling_Report;
      SR : Sources.Controller.Sampling_Report;
      Search : Batteries.Controller.Discovery_Report;
      Battery : Batteries.Controller.Observation_Report;
      Cursor, Device : Core.Namespace.Node_ID := Core.Namespace.Root;
      Admitted : Boolean;
      type Phase is (Lid_Sampling, Source_Sampling, Battery_Search, Battery_Sampling);
      Stage : Phase := Lid_Sampling;
      procedure Finish (Status : Outcome) is
      begin
         Saved.Status := Status;
         if Saved.Recovery_Needed then
            case Stage is
               when Lid_Sampling =>
                  ACPI_Native_Log.Warning ("acpi: stage power-owner fail RECOVERY_REQUIRED" &
                    " identity=" & LR.Search.Last_Identity.Outcome'Image &
                    " identity-exec=" & LR.Search.Last_Identity.Execution'Image &
                    " status=" & LR.Device_Status.Outcome'Image &
                    " status-exec=" & LR.Device_Status.Execution'Image &
                    " query=" & LR.Query.Outcome'Image & " query-exec=" & LR.Query.Execution'Image);
               when Source_Sampling =>
                  ACPI_Native_Log.Warning ("acpi: stage power-owner fail RECOVERY_REQUIRED" &
                    " identity=" & SR.Search.Last_Identity.Outcome'Image &
                    " identity-exec=" & SR.Search.Last_Identity.Execution'Image &
                    " status=" & SR.Device_Status.Outcome'Image &
                    " status-exec=" & SR.Device_Status.Execution'Image &
                    " query=" & SR.Query.Outcome'Image & " query-exec=" & SR.Query.Execution'Image);
               when Battery_Search =>
                  ACPI_Native_Log.Warning ("acpi: stage power-owner fail RECOVERY_REQUIRED" &
                    " identity=" & Search.Last_Identity.Outcome'Image &
                    " execution=" & Search.Last_Identity.Execution'Image);
               when Battery_Sampling =>
                  ACPI_Native_Log.Warning ("acpi: stage power-owner fail RECOVERY_REQUIRED" &
                    " result=" & Battery.Status'Image & " phase=" & Battery.Reached'Image);
            end case;
         end if;
         if Status in Finished | No_Devices then
            ACPI_Native_Log.Info ("acpi: stage power-discovery ok " & Status'Image &
              " lid=" & Boolean'Image (Saved.Lid_Observed) &
              " ac=" & Boolean'Image (Saved.Source_Observed) &
              " batteries=" & Saved.Batteries'Image & " sampled=" & Saved.Sampled_Batteries'Image &
              " absent=" & Saved.Absent_Batteries'Image & " fuel=" & Saved.Charged'Image);
         else
            ACPI_Native_Log.Warning ("acpi: stage power-discovery fail " & Status'Image &
              " phase=" & Stage'Image & " fuel=" & Saved.Charged'Image &
              " lid-failed=" & Boolean'Image (Saved.Lid_Query_Failed) &
              " ac-failed=" & Boolean'Image (Saved.Source_Query_Failed) &
              " sampled=" & Saved.Sampled_Batteries'Image &
              " recovery=" & Boolean'Image (Saved.Recovery_Needed));
         end if;
         Result := Saved;
      end Finish;
   begin
      if Attempted then Result := Saved; return; end if;
      Attempted := True;
      Saved.Status := Owner_Error;
      Saved.Recovery_Needed := True;
      ACPI_Requests.Sleep_Registers.Observe (Server, Saved.Sleep_Registers);
      declare
         package P renames ACPI_FADT.Sleep_Registers;
         use type P.Source_Status, P.Selection_Status;
         Line : constant String := "acpi: stage sleep-registers " &
           (if Saved.Sleep_Registers.Status = P.Classified and then Saved.Sleep_Registers.Data.Code = P.Selected
            then "ok " else "fail ") & Saved.Sleep_Registers.Status'Image &
           " selection=" & Saved.Sleep_Registers.Data.Code'Image &
           " model=" & Saved.Sleep_Registers.Data.Model'Image &
           " pm1a=" & Saved.Sleep_Registers.Data.A'Image & " pm1b=" & Saved.Sleep_Registers.Data.B'Image &
           " low-power-idle=" & Boolean'Image (Saved.Sleep_Registers.Data.Low_Power_Idle);
      begin
         if Saved.Sleep_Registers.Status = P.Classified and then Saved.Sleep_Registers.Data.Code = P.Selected then
            ACPI_Native_Log.Info (Line);
         else ACPI_Native_Log.Warning (Line);
         end if;
      end;
      Lids.Sample_Ready (Server, Limits.Fuel, LR);
      Saved.Charged := LR.Charged;
      Saved.Recovery_Needed := LR.Outcome = Lids.Controller.Provider_Contract_Error
        or else LR.Search.Last_Identity.Outcome in Lid_Driver.Cleanup_Error | Lid_Driver.Evaluator_Error
        or else LR.Device_Status.Outcome in Lid_Driver.Device_Status.Cleanup_Failed |
          Lid_Driver.Device_Status.Provider_Contract_Error
        or else LR.Query.Outcome in Lid_Driver.Cleanup_Failed | Lid_Driver.Provider_Contract_Error
        or else Uncertain (LR.Search.Last_Identity.Execution) or else Uncertain (LR.Device_Status.Execution)
        or else Uncertain (LR.Query.Execution);
      if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
      case LR.Outcome is
         when Lids.Controller.Not_Ready => Finish (Not_Ready); return;
         when Lids.Controller.Budget_Exhausted => Finish (Budget_Exhausted); return;
         when Lids.Controller.No_Match => null;
         when Lids.Controller.Sampled =>
            Saved.Lid_Observed := True;
            Saved.Lid := (case LR.Query.State is
              when Lid_Driver.Unknown => Unknown, when Lid_Driver.Closed => Closed, when Lid_Driver.Open => Open);
            ACPI_Native_Log.Info ("acpi: stage lid-observation ok OBSERVED state=" & Saved.Lid'Image &
              " raw=" & LR.Query.Raw'Image);
         when others =>
            ACPI_Native_Log.Warning ("acpi: stage lid-observation fail " & LR.Outcome'Image &
              " identity=" & LR.Search.Last_Identity.Outcome'Image &
              " identity-exec=" & LR.Search.Last_Identity.Execution'Image &
              " status=" & LR.Device_Status.Outcome'Image & " flags=" & LR.Device_Status.Flags'Image &
              " status-exec=" & LR.Device_Status.Execution'Image &
              " query=" & LR.Query.Outcome'Image & " execution=" & LR.Query.Execution'Image);
            Saved.Lid_Query_Failed := True;
      end case;
      Stage := Source_Sampling;
      Saved.Recovery_Needed := True;
      Sources.Sample_Ready (Server, Limits.Fuel - Saved.Charged, SR);
      Saved.Charged := Saved.Charged + SR.Charged;
      Saved.Recovery_Needed := SR.Outcome = Sources.Controller.Provider_Contract_Error
        or else SR.Search.Last_Identity.Outcome in Source_Driver.Cleanup_Failed | Source_Driver.Provider_Contract_Error
        or else SR.Device_Status.Outcome in Source_Driver.Device_Status.Cleanup_Failed |
          Source_Driver.Device_Status.Provider_Contract_Error
        or else SR.Query.Outcome in Source_Driver.Cleanup_Failed | Source_Driver.Provider_Contract_Error
        or else Uncertain (SR.Search.Last_Identity.Execution) or else Uncertain (SR.Device_Status.Execution)
        or else Uncertain (SR.Query.Execution);
      if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
      case SR.Outcome is
         when Sources.Controller.Not_Ready => Finish (Not_Ready); return;
         when Sources.Controller.Budget_Exhausted => Finish (Budget_Exhausted); return;
         when Sources.Controller.No_Match => null;
         when Sources.Controller.Sampled =>
            Saved.Source_Observed := True;
            Saved.Source := (case SR.Query.State is
              when Source_Driver.Unknown => Unknown, when Source_Driver.Offline => Offline, when Source_Driver.Online => Online);
            ACPI_Native_Log.Info ("acpi: stage ac-observation ok OBSERVED state=" & Saved.Source'Image &
              " raw=" & SR.Query.Raw'Image);
         when others =>
            ACPI_Native_Log.Warning ("acpi: stage ac-observation fail " & SR.Outcome'Image &
              " identity=" & SR.Search.Last_Identity.Outcome'Image &
              " identity-exec=" & SR.Search.Last_Identity.Execution'Image &
              " status=" & SR.Device_Status.Outcome'Image & " flags=" & SR.Device_Status.Flags'Image &
              " status-exec=" & SR.Device_Status.Execution'Image &
              " query=" & SR.Query.Outcome'Image & " execution=" & SR.Query.Execution'Image);
            Saved.Source_Query_Failed := True;
      end case;
      loop
         Stage := Battery_Search;
         Saved.Recovery_Needed := True;
         Batteries.Find (Server, Cursor, Limits.Fuel - Saved.Charged, Device, Search);
         Saved.Charged := Saved.Charged + Search.Charged;
         Saved.Recovery_Needed := Search.Last_Identity.Outcome in Dynamic.Cleanup_Failed | Dynamic.Provider_Contract_Error
           or else Uncertain (Search.Last_Identity.Execution);
         if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
         case Search.Outcome is
            when Batteries.Controller.No_Match =>
               Finish (if Saved.Lid_Query_Failed then Lid_Failed
                 elsif Saved.Source_Query_Failed then Source_Failed
                 elsif not Saved.Lid_Observed and not Saved.Source_Observed and Saved.Batteries = 0
                 then No_Devices else Finished); return;
            when Batteries.Controller.Not_Ready => Finish (Not_Ready); return;
            when Batteries.Controller.Incomplete => Finish (Budget_Exhausted); return;
            when Batteries.Controller.Found => null;
            when others =>
               ACPI_Native_Log.Warning ("acpi: stage battery-discovery fail " & Search.Outcome'Image &
                 " identity=" & Search.Last_Identity.Outcome'Image &
                 " execution=" & Search.Last_Identity.Execution'Image);
               Finish (Battery_Failed); return;
         end case;
         Saved.Batteries := Saved.Batteries + 1;
         Stage := Battery_Sampling;
         Saved.Recovery_Needed := True;
         Batteries.Observe (Server, Device, Limits.Fuel - Saved.Charged, Admitted, Battery);
         Saved.Charged := Saved.Charged + Battery.Charged;
         Saved.Recovery_Needed := Battery.Status in Collector.Cleanup_Failed | Collector.Provider_Contract_Error
           or else Uncertain (Battery.Identity.Execution) or else Uncertain (Battery.Before_Presence.Execution)
           or else Uncertain (Battery.Static_Data.Execution) or else Uncertain (Battery.Dynamic_Data.Execution)
           or else Uncertain (Battery.After_Presence.Execution);
         if Saved.Recovery_Needed then Finish (Recovery_Required); return; end if;
         if not Admitted then Finish (Not_Ready); return; end if;
         if Battery.Status = Collector.Budget_Exhausted then Finish (Budget_Exhausted); return; end if;
         if Battery.Status = Collector.Absent then
            Saved.Absent_Batteries := Saved.Absent_Batteries + 1;
            ACPI_Native_Log.Info ("acpi: stage battery-observation ok ABSENT node=" & Device'Image);
         elsif Battery.Status = Collector.Sampled then
            Saved.Sampled_Batteries := Saved.Sampled_Batteries + 1;
            ACPI_Native_Log.Info ("acpi: stage battery-observation ok OBSERVED node=" & Device'Image &
              " flow=" & Battery.Value.Status.Flow'Image &
              " remaining-known=" & Boolean'Image (Battery.Value.Status.Remaining_Capacity.Known) &
              " remaining=" & Battery.Value.Status.Remaining_Capacity.Value'Image &
              " units=" & Battery.Value.Information.Units'Image);
         else
            ACPI_Native_Log.Warning ("acpi: stage battery-observation fail " & Battery.Status'Image &
              " phase=" & Battery.Reached'Image & " information=" & Battery.Static_Data.Outcome'Image &
              " dynamic=" & Battery.Dynamic_Data.Outcome'Image);
            Finish (Battery_Failed); return;
         end if;
         if Saved.Batteries = Limits.Batteries then Finish (Battery_Limit); return; end if;
         Cursor := Device;
      end loop;
   end Run;
end ACPI_Native_Power_Detection;
