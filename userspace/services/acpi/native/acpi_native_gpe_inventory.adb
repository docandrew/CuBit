pragma Ada_2022;
with ACPI_One_Service; with ACPI_SCI_Events;
package body ACPI_Native_GPE_Inventory is
   package Q renames ACPI_Requests;
   package One renames ACPI_One_Service;
   package G renames One.GPE_Events;
   package R renames ACPI_SCI_Registers;
   use type Q.Phase, One.Runtime.Status.Phase;
   use type G.Requests.Controller.Admission, Methods.Resolution_Outcome;
   Last : Report;
   States : array (R.GPE_Number) of Entry_Status := [others => Unobserved];
   Bindings : array (R.GPE_Number) of Methods.Binding := [others => Methods.No_Binding];
   function Observe return Report is (Last);
   function Status (GPE : R.GPE_Number) return Entry_Status is (States (GPE));
   procedure Configure (Server : Q.State; EC_Present : Boolean;
      EC_GPE : R.GPE_Number; Accepted : out Boolean) is
      Allowed : G.Requests.Controller.Admission;
      Item : Methods.Binding;
      Resolution : Methods.Resolution_Report;
   begin
      Accepted := False;
      if Last.Attempted then return; end if;
      if not Q.Valid (Server) or else Q.Current (Server) /= Q.Complete
        or else not Q.Device_Walk_Finished (Server) or else not Q.Notifications_Ready (Server)
        or else Q.Notifications_Pending (Server) or else Q.Retained_Results_Pending (Server)
        or else One.Runtime.Current /= One.Runtime.Status.Idle
        or else not One.Root.Fields.Control_Idle or else not One.Root.Fields.EC_Commands.Quiescent
        or else not ACPI_SCI_Events.Is_Armed
      then Last.Why := Not_Ready; return; end if;
      Last.Attempted := True; Last.Why := None;
      if EC_Present and then not ACPI_SCI_Events.GPE_Available (EC_GPE) then
         Last.Stage := Failed; Last.Why := EC_Source_Unavailable; return;
      end if;
      for GPE in R.GPE_Number loop
         if not ACPI_SCI_Events.GPE_Available (GPE) then States (GPE) := Not_Available;
         else
            Last.Available := Last.Available + 1;
            if EC_Present and then GPE = EC_GPE then
               States (GPE) := EC_Source; Last.EC_Excluded := Last.EC_Excluded + 1;
            else
               G.Requests.Resolve (Server, Methods.Event_Number (GPE), Item, Allowed, Resolution);
               if Allowed /= G.Requests.Controller.Admitted then
                  Last.Stage := Failed; Last.Why := Owner_Refused; return;
               end if;
               case Resolution.Status is
                  when Methods.Found =>
                     Bindings (GPE) := Item; States (GPE) := Handler_Ready; Last.Bound := Last.Bound + 1;
                  when Methods.Missing => States (GPE) := No_Handler; Last.Missing := Last.Missing + 1;
                  when others =>
                     States (GPE) := Handler_Declined; Last.Declined := Last.Declined + 1;
                     if not Last.Has_Declined then
                        Last.Has_Declined := True; Last.First_Declined := GPE;
                        Last.Decline_Reason := Resolution.Status;
                     end if;
               end case;
            end if;
         end if;
      end loop;
      Last.Stage := Ready; Accepted := True;
   exception
      when others =>
         Last.Attempted := True; Last.Stage := Failed; Last.Why := Metadata_Exception; Accepted := False;
   end Configure;
   procedure Lookup (GPE : R.GPE_Number; Item : out Methods.Binding; Found : out Boolean) is
   begin
      Item := Methods.No_Binding;
      Found := Last.Stage = Ready and then States (GPE) = Handler_Ready;
      if Found then Item := Bindings (GPE); end if;
   end Lookup;
end ACPI_Native_GPE_Inventory;
