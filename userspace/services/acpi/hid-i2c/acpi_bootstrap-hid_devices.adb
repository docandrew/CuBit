pragma Ada_2022;
with HID_I2C_Descriptors;
package body ACPI_Bootstrap.HID_Devices with SPARK_Mode is
   package C renames ACPI_Service;
   package N renames C.Namespace;
   use type N.Object_Kind, Discovery.Queries.Identification_Outcome;
   use type Discovery.Action_Result, Discovery.Outcome;
   use type Discovery.Resources.R.Byte_Count, Resolver.Outcome;
   function Ready (Session : State) return Boolean is
     (Session.Stage = Complete and then Session.Device_Data.Stage = Devices_Completed);
   procedure Find
     (Session : in out State; After : N.Node_ID; Budget : Natural;
      Device : out N.Node_ID; Result : out Search_Report)
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
      Device := N.Root; Result := (others => <>);
      if not Ready (Session) then return; end if;
      Last := C.Namespace_Node_Count (Session.Core);
      if After > Last then Result.Outcome := Invalid_Cursor; return; end if;
      Result.Outcome := Incomplete;
      if Budget = 0 then return; end if;
      for I in N.Root .. Last loop
         C.Describe_Node (Session.Core, I, Snapshot (I), Status);
         if Status /= C.Values.Available then Result.Outcome := Failed; return; end if;
      end loop;
      for I in N.Root .. Last loop
         if I > After then
            if Result.Charged = Budget then return; end if;
            Result.Scanned := Result.Scanned + 1;
            if C.Values.Node_Kind (Snapshot (I)) = N.Device_Object then
               Result.Candidates := Result.Candidates + 1;
               Discovery.Queries.Identify (Session.Core, Snapshot (I), Budget - Result.Charged, Result.Last_Identity);
               if Result.Last_Identity.Charged > Budget - Result.Charged then Result.Outcome := Failed; return; end if;
               Result.Charged := Result.Charged + Result.Last_Identity.Charged;
               if not Same_Topology then Result.Outcome := Failed; return; end if;
               case Result.Last_Identity.Outcome is
                  when Discovery.Queries.Compatible => Device := I; Result.Outcome := Found; return;
                  when Discovery.Queries.Not_Compatible | Discovery.Queries.Missing_CID => null;
                  when Discovery.Queries.Budget_Exhausted => return;
                  when others => Result.Outcome := Failed; return;
               end case;
            end if;
         end if;
      end loop;
      Result.Outcome := No_Match;
   end Find;
   function Pending (Run : Probe) return Boolean is (Discovery.Pending (Run.Work));
   function Observe (Run : Probe) return Report is
     (Discovery.Observe (Run.Work), Run.Tried, Run.Resolution);
   function Current (Run : Probe; Session : State) return Boolean is
     (Ready (Session) and then Run.Tried and then Run.Resolution.Status = Resolver.Resolved
      and then Resolver.Current (Session.Core, Run.Resolution.Value));
   procedure Begin_Probe
     (Run : in out Probe; Session : in out State;
      Device : ACPI_Service.Namespace.Node_ID; Budget : Natural;
      Allowed : out Admission; Action : out Discovery.Action_Result)
   is
      Item : ACPI_Service.Node_Descriptor;
      Status : ACPI_Service.Values.Access_Status;
   begin
      Allowed := Not_Ready; Action := Discovery.Not_Pending;
      if not Ready (Session) then return; end if;
      Allowed := Admitted; Action := Discovery.Busy;
      if Pending (Run) then return; end if;
      ACPI_Service.Describe_Node (Session.Core, Device, Item, Status);
      if Status /= ACPI_Service.Values.Available then Item := ACPI_Service.Values.No_Node; end if;
      Discovery.Begin_Discovery (Run.Work, Session.Core, Item, Budget, Action);
      if Action = Discovery.Accepted then
         Run.Device := Item; Run.Tried := False; Run.Resolution := (others => <>);
      end if;
   end Begin_Probe;
   procedure Begin_Next_Probe
     (Run : in out Probe; Session : in out State;
      After : N.Node_ID; Budget : Natural;
      Device : out N.Node_ID; Search : out Search_Report;
      Allowed : out Admission; Action : out Discovery.Action_Result) is
   begin
      Device := N.Root; Search := (others => <>);
      Allowed := Not_Ready; Action := Discovery.Not_Pending;
      if not Ready (Session) then return; end if;
      Allowed := Admitted;
      if Pending (Run) then Action := Discovery.Busy; return; end if;
      Find (Session, After, Budget, Device, Search);
      if Search.Outcome /= Found then return; end if;
      Begin_Probe (Run, Session, Device, Budget - Search.Charged, Allowed, Action);
   end Begin_Next_Probe;
   procedure Copy_And_Resolve
     (Run : in out Probe; Session : in out State; Data : out Firmware_Tables.Bytes;
      Allowed : out Admission; Action : out Discovery.Action_Result)
   is
      Info : Discovery.Report;
   begin
      Data := [others => 0]; Allowed := Not_Ready; Action := Discovery.Not_Pending;
      if not Ready (Session) then return; end if;
      Allowed := Admitted;
      Discovery.Copy_And_Finish (Run.Work, Session.Core, Data, Action);
      Info := Discovery.Observe (Run.Work);
      if Action = Discovery.Accepted and then Info.Result = Discovery.Observed then
         if Info.Copy.Copied = 0
           or else Info.Copy.Copied > Discovery.Resources.R.Byte_Count (Data'Length) then return; end if;
         Run.Tried := True;
         Resolver.Resolve (Session.Core, Run.Device,
           Data (Data'First .. Data'First + (Natural (Info.Copy.Copied) - 1)),
           HID_I2C_Descriptors.Register_Number (Info.Descriptor.Address), Run.Resolution);
      end if;
   end Copy_And_Resolve;
   procedure Cancel
     (Run : in out Probe; Session : in out State;
      Action : out Discovery.Action_Result) is
   begin
      Discovery.Cancel (Run.Work, Session.Core, Action);
   end Cancel;
end ACPI_Bootstrap.HID_Devices;
