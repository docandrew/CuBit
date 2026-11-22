pragma Ada_2022;
package body ACPI_Bootstrap.Battery_Observations with SPARK_Mode is
   package C renames ACPI_Service;
   package N renames C.Namespace;
   use type N.Object_Kind, Identity.Identification_Outcome;
   function Ready (Session : State) return Boolean is
     (Session.Stage = Complete and then Session.Device_Data.Stage = Devices_Completed);
   procedure Find
     (Session : in out State; After : N.Node_ID; Budget : Natural;
      Device : out N.Node_ID; Report : out Discovery_Report)
   is
      type Descriptors is array (N.Node_ID) of C.Node_Descriptor;
      Snapshot : Descriptors := [others => C.Values.No_Node];
      Last : N.Node_ID;
      Status : C.Values.Access_Status;
      function Same_Topology return Boolean is
      begin
         if not Ready (Session) or else C.Namespace_Node_Count (Session.Core) /= Last then return False; end if;
         for I in N.Root .. Last loop
            if not C.Current_Node (Session.Core, Snapshot (I)) then return False; end if;
         end loop;
         return True;
      end Same_Topology;
   begin
      Device := N.Root; Report := (others => <>);
      if not Ready (Session) then return; end if;
      Last := C.Namespace_Node_Count (Session.Core);
      if After > Last then Report.Outcome := Invalid_Cursor; return; end if;
      Report.Outcome := Incomplete;
      if Budget = 0 then return; end if;
      for I in N.Root .. Last loop
         C.Describe_Node (Session.Core, I, Snapshot (I), Status);
         if Status /= C.Values.Available then Report.Outcome := Failed; return; end if;
      end loop;
      for I in N.Root .. Last loop
         if I > After then
            if Report.Charged = Budget then return; end if;
            Report.Scanned := Report.Scanned + 1;
            if C.Values.Node_Kind (Snapshot (I)) = N.Device_Object then
               Report.Candidates := Report.Candidates + 1;
               Identity.Identify (Session.Core, Snapshot (I), Budget - Report.Charged, Report.Last_Identity);
               if Report.Last_Identity.Charged > Budget - Report.Charged then Report.Outcome := Failed; return; end if;
               Report.Charged := Report.Charged + Report.Last_Identity.Charged;
               if not Same_Topology then Report.Outcome := Failed; return; end if;
               case Report.Last_Identity.Outcome is
                  when Identity.Matched => Device := I; Report.Outcome := Found; return;
                  when Identity.Not_Battery | Identity.Missing_HID => null;
                  when Identity.Budget_Exhausted => return;
                  when others => Report.Outcome := Failed; return;
               end case;
            end if;
         end if;
      end loop;
      Report.Outcome := No_Match;
   end Find;
   procedure Observe (Session : in out State; Device : N.Node_ID;
      Budget : Natural; Admitted : out Boolean; Report : out Observation_Report) is
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      Admitted := False; Report := (Status => Collector.Not_Ready, others => <>);
      if not Ready (Session) then return; end if;
      Admitted := True;
      C.Describe_Node (Session.Core, Device, Item, Status);
      if Status /= C.Values.Available then
         Report := (Status => Collector.Identification_Failed, others => <>); return;
      end if;
      Collector.Query (Session.Core, Item, Budget, Report);
   end Observe;
end ACPI_Bootstrap.Battery_Observations;
