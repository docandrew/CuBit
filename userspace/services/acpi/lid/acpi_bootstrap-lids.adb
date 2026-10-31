pragma Ada_2022;
package body ACPI_Bootstrap.Lids with SPARK_Mode is
   package C renames ACPI_Service;
   package V renames C.Values;
   package N renames C.Namespace;
   use type N.Object_Kind;
   use type Driver.Identification_Outcome;
   use type AML_Execute.Execution_Status;
   function Ready (Session : State) return Boolean is
     (Session.Stage = Complete and then Session.Device_Data.Stage = Devices_Completed);
   procedure Find (Session : in out State; Budget : Natural;
      Item : out Binding; Report : out Discovery_Report) is
      type Descriptors is array (N.Node_ID) of C.Node_Descriptor;
      Snapshot : Descriptors := [others => V.No_Node];
      Last : N.Node_ID;
      Status : V.Access_Status;
      function Same_Topology return Boolean is
      begin
         if not Ready (Session) or else C.Namespace_Node_Count (Session.Core) /= Last then
            return False;
         end if;
         for I in N.Root .. Last loop
            if not C.Current_Node (Session.Core, Snapshot (I)) then return False; end if;
         end loop;
         return True;
      end Same_Topology;
   begin
      Item := No_Binding; Report := (others => <>);
      if not Ready (Session) then return; end if;
      Report.Outcome := Incomplete;
      if Budget = 0 then return; end if;
      Last := C.Namespace_Node_Count (Session.Core);
      for I in N.Root .. Last loop
         C.Describe_Node (Session.Core, I, Snapshot (I), Status);
         if Status /= V.Available then Report.Outcome := Failed; return; end if;
      end loop;
      for I in N.Root .. Last loop
         if Report.Charged = Budget then return; end if;
         Report.Scanned := Report.Scanned + 1;
         if V.Node_Kind (Snapshot (I)) = N.Device_Object then
            Report.Candidates := Report.Candidates + 1;
            Driver.Identify (Session.Core, Snapshot (I), Budget - Report.Charged, Report.Last_Identity);
            if Report.Last_Identity.Charged > Budget - Report.Charged then
               Report.Outcome := Failed; return;
            end if;
            Report.Charged := Report.Charged + Report.Last_Identity.Charged;
            if not Same_Topology then Report.Outcome := Failed; return; end if;
            case Report.Last_Identity.Outcome is
               when Driver.Matched =>
                  Item.Device := Snapshot (I); Report.Outcome := Found; return;
               when Driver.Not_Lid | Driver.No_HID => null;
               when Driver.No_Budget => return;
               when Driver.Wrong_HID_Type | Driver.Evaluation_Error =>
                  Report.Errors := Report.Errors + 1;
                  if Report.Last_Identity.Execution in AML_Execute.Field_Indeterminate | AML_Execute.Fatal_Requested then
                     Report.Outcome := Failed; return;
                  end if;
               when others => Report.Outcome := Failed; return;
            end case;
         end if;
      end loop;
      Report.Outcome := (if Report.Errors = 0 then No_Match else Failed);
   end Find;
   procedure Query (Session : in out State; Item : Binding; Budget : Natural;
      Admission : out Query_Admission; Report : out Driver.Query_Report) is
   begin
      Report := (others => <>);
      Admission := Not_Ready;
      if not Ready (Session) then return; end if;
      Admission := Invalid_Binding;
      if not C.Current_Node (Session.Core, Item.Device)
        or else V.Node_Kind (Item.Device) /= N.Device_Object
      then
         return;
      end if;
      Admission := Admitted;
      Driver.Query (Session.Core, Item.Device, Budget, Report);
   end Query;
   procedure Sample_Ready (Session : in out State; Budget : Natural;
      Report : out Sampling_Report) is
      Item : Binding;
      Admission : Query_Admission;
      use type Driver.Device_Status.Status_Outcome, Driver.Query_Outcome;
   begin
      Report := (others => <>);
      Find (Session, Budget, Item, Report.Search);
      Report.Charged := Report.Search.Charged;
      case Report.Search.Outcome is
         when Not_Ready => Report.Outcome := Not_Ready; return;
         when No_Match => Report.Outcome := No_Match; return;
         when Incomplete => Report.Outcome := Budget_Exhausted; return;
         when Failed => Report.Outcome := Search_Failed; return;
         when Found => null;
      end case;
      Driver.Device_Status.Observe_Status
        (Session.Core, Item.Device, Budget - Report.Charged, Report.Device_Status);
      Report.Charged := Report.Charged + Report.Device_Status.Charged;
      if Report.Device_Status.Outcome /= Driver.Device_Status.Status_Available then
         Report.Outcome := (if Report.Device_Status.Outcome = Driver.Device_Status.Budget_Exhausted
           then Budget_Exhausted else Status_Failed); return;
      end if;
      if not Driver.Device_Status.Usable (Report.Device_Status) then
         Report.Outcome := Device_Unavailable; return;
      end if;
      Query (Session, Item, Budget - Report.Charged, Admission, Report.Query);
      if Report.Query.Charged > Budget - Report.Charged then
         Report.Outcome := Provider_Contract_Error; return;
      end if;
      Report.Charged := Report.Charged + Report.Query.Charged;
      Report.Outcome := (if Admission /= Admitted then Not_Ready
        elsif Report.Query.Outcome = Driver.Sampled then Sampled
        elsif Report.Query.Outcome = Driver.Budget_Exhausted then Budget_Exhausted
        else Query_Failed);
   end Sample_Ready;
end ACPI_Bootstrap.Lids;
