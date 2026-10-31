with AML_Decode;
package body ACPI_Battery_Observation with SPARK_Mode is
   use type Dynamic.Identification_Outcome;
   use type Dynamic.Presence_Outcome;
   use type Dynamic.Query_Outcome;
   use type Static_Info.Query_Outcome;
   use type AML_Decode.Integer_Value;
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      function Add_Charge (Amount : Natural) return Boolean is
      begin
         if Amount > Budget - Report.Charged then
            Report.Status := Provider_Contract_Error; return False;
         end if;
         Report.Charged := Report.Charged + Amount;
         return True;
      end Add_Charge;
      function Check_Presence (Item : Dynamic.Presence_Report) return Boolean is
      begin
         if Item.Outcome /= Dynamic.Observed then
            Report.Status := (case Item.Outcome is
              when Dynamic.Budget_Exhausted => Budget_Exhausted,
              when Dynamic.Cleanup_Failed => Cleanup_Failed,
              when Dynamic.Provider_Contract_Error => Provider_Contract_Error,
              when others => Presence_Failed);
            return False;
         end if;
         -- ACPI6.6 6.3.7: enabled without presence is invalid. Functioning
         -- without enabled is valid only for the absent/disabled bridge case.
         -- Invalid firmware status is unknown, never an absent-battery sample.
         if (Item.Enabled and then not Item.Device_Present)
           or else (Item.Functioning and then not Item.Enabled and then Item.Device_Present)
         then
            Report.Status := Presence_Failed; return False;
         end if;
         return True;
      end Check_Presence;
      procedure Run is
      begin
         if Budget = 0 then Report.Status := Budget_Exhausted; return; end if;
         Report.Attempts := 1;
         Dynamic.Identify (Service, Device, Budget, Report.Identity);
         if not Add_Charge (Report.Identity.Charged) then return; end if;
         if Report.Identity.Outcome /= Dynamic.Matched then
            Report.Status := (case Report.Identity.Outcome is
              when Dynamic.Not_Battery => Not_Battery,
              when Dynamic.Budget_Exhausted => Budget_Exhausted,
              when Dynamic.Cleanup_Failed => Cleanup_Failed,
              when Dynamic.Provider_Contract_Error => Provider_Contract_Error,
              when others => Identification_Failed);
            return;
         end if;
         Report.Reached := Presence; Report.Attempts := 2;
         Dynamic.Query_Presence (Service, Device, Budget - Report.Charged, Report.Before_Presence);
         if not Add_Charge (Report.Before_Presence.Charged) or else not Check_Presence (Report.Before_Presence) then return; end if;
         if not Report.Before_Presence.Device_Present or else not Report.Before_Presence.Battery_Present then
            Report.Status := Absent; return;
         elsif not Report.Before_Presence.Enabled or else not Report.Before_Presence.Functioning then
            Report.Status := Not_Ready; return;
         end if;
         Report.Reached := Information; Report.Attempts := 3;
         Static_Info.Query (Service, Device, Budget - Report.Charged, Report.Static_Data);
         if not Add_Charge (Report.Static_Data.Charged) then return; end if;
         if Report.Static_Data.Outcome /= Static_Info.Sampled then
            Report.Status := (case Report.Static_Data.Outcome is
              when Static_Info.Budget_Exhausted => Budget_Exhausted,
              when Static_Info.Cleanup_Failed => Cleanup_Failed,
              when Static_Info.Provider_Contract_Error => Provider_Contract_Error,
              when others => Information_Failed);
            return;
         end if;
         Report.Reached := Dynamic_Status; Report.Attempts := 4;
         Dynamic.Query (Service, Device, Budget - Report.Charged, Report.Dynamic_Data);
         if not Add_Charge (Report.Dynamic_Data.Charged) then return; end if;
         if Report.Dynamic_Data.Outcome /= Dynamic.Sampled then
            Report.Status := (case Report.Dynamic_Data.Outcome is
              when Dynamic.Budget_Exhausted => Budget_Exhausted,
              when Dynamic.Cleanup_Failed => Cleanup_Failed,
              when Dynamic.Provider_Contract_Error => Provider_Contract_Error,
              when others => Status_Failed);
            return;
         end if;
         Report.Reached := Confirm_Presence; Report.Attempts := 5;
         Dynamic.Query_Presence (Service, Device, Budget - Report.Charged, Report.After_Presence);
         if not Add_Charge (Report.After_Presence.Charged) or else not Check_Presence (Report.After_Presence) then return; end if;
         if Report.After_Presence.Flags /= Report.Before_Presence.Flags
           or else Report.After_Presence.Defaulted /= Report.Before_Presence.Defaulted
         then Report.Status := Presence_Changed; return; end if;
         Report.Value := (Information => Report.Static_Data.Value, Status => Report.Dynamic_Data.Value);
         Report.Status := Sampled;
         Report.Reached := Finished;
      end Run;
   begin
      Report := (others => <>);
      Run;
      if Report.Status /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before_Roots then
         Report.Status := Provider_Contract_Error;
      end if;
      if Report.Status /= Sampled then Report.Value := Empty_Observation; end if;
   end Query;
end ACPI_Battery_Observation;
