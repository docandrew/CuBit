pragma Ada_2022;
with GPIO_Community_Window;
with GPIO_Kernel_Regions;
with GPIO_Native_Clock;
package body GPIO_Native_Communities is
   use type P.Address;
   use type P.Outcome;
   use type T.Map_Outcome;
   use type T.Header_Outcome;
   use type G.Outcome;
   use type I.Tick;
   package R0 is new GPIO_Community_Window (CuBit.Handles.Device_Memory, GPIO_Kernel_Regions.Inspect, GPIO_Kernel_Regions.Map);
   package R1 is new GPIO_Community_Window (CuBit.Handles.Device_Memory, GPIO_Kernel_Regions.Inspect, GPIO_Kernel_Regions.Map);
   package R2 is new GPIO_Community_Window (CuBit.Handles.Device_Memory, GPIO_Kernel_Regions.Inspect, GPIO_Kernel_Regions.Map);
   package R3 is new GPIO_Community_Window (CuBit.Handles.Device_Memory, GPIO_Kernel_Regions.Inspect, GPIO_Kernel_Regions.Map);
   type Resource_State is record
      Attempted, Mapped, Probe_Attempted, Probed : Boolean := False;
      Physical, Virtual, Bytes : P.Address := 0;
   end record;
   Resources : array (G.Memory_Resource_Ordinal) of Resource_State;
   Frozen : Boolean := False;
   function Ready (Resource : G.Memory_Resource_Ordinal) return Boolean is
     (Frozen and Resources (Resource).Probed);
   procedure Bind
     (Resource : G.Memory_Resource_Ordinal; Authority : CuBit.Handles.Device_Memory;
      Physical, Bytes, Virtual, Page_Limit : P.Address; Deadline : I.Tick;
      Result : out T.Map_Report) is
      Metadata : P.Description;
      Now : I.Tick;
      Clock_OK : Boolean;
   begin
      Result := (others => <>);
      if Frozen then Result.Status := T.Setup_Closed; return; end if;
      if Resources (Resource).Attempted then Result.Status := T.Already_Attempted; return; end if;
      Resources (Resource).Attempted := True;
      GPIO_Native_Clock.Read (Now, Clock_OK);
      if not Clock_OK then Result.Status := T.Clock_Refused; return; end if;
      if Now >= Deadline then Result.Status := T.Expired; return; end if;
      GPIO_Kernel_Regions.Inspect (Authority, Metadata);
      Result.Policy := P.Admit (Metadata, Physical, Bytes, Virtual, Page_Limit);
      if Result.Policy /= P.Allowed then return; end if;
      for Prior of Resources loop
         if Prior.Mapped and then
           ((Physical < Prior.Physical + Prior.Bytes and then Prior.Physical < Physical + Bytes)
            or else (Virtual < Prior.Virtual + Prior.Bytes and then Prior.Virtual < Virtual + Bytes))
         then Result.Status := T.Resource_Overlap; return; end if;
      end loop;
      case Resource is
         when 0 => R0.Bind (Authority, Resource, Physical, Bytes, Virtual, Page_Limit, Result);
         when 1 => R1.Bind (Authority, Resource, Physical, Bytes, Virtual, Page_Limit, Result);
         when 2 => R2.Bind (Authority, Resource, Physical, Bytes, Virtual, Page_Limit, Result);
         when 3 => R3.Bind (Authority, Resource, Physical, Bytes, Virtual, Page_Limit, Result);
      end case;
      if Result.Status = T.Bound then
         Resources (Resource).Mapped := True;
         Resources (Resource).Physical := Physical;
         Resources (Resource).Virtual := Virtual;
         Resources (Resource).Bytes := Bytes;
      end if;
   end Bind;
   procedure Probe
     (Resource : G.Memory_Resource_Ordinal; Controller_HID : String;
      Deadline : I.Tick; Result : out T.Header_Report) is
      Now : I.Tick;
      Clock_OK : Boolean;
   begin
      Result := (others => <>);
      if Frozen then Result.Status := T.Setup_Closed; return; end if;
      if not Resources (Resource).Mapped then return; end if;
      if Resources (Resource).Probe_Attempted then Result.Status := T.Already_Probed; return; end if;
      Resources (Resource).Probe_Attempted := True;
      GPIO_Native_Clock.Read (Now, Clock_OK);
      if not Clock_OK then Result.Status := T.Clock_Refused; return; end if;
      if Now >= Deadline then Result.Status := T.Expired; return; end if;
      case Resource is
         when 0 => R0.Probe (Controller_HID, Result);
         when 1 => R1.Probe (Controller_HID, Result);
         when 2 => R2.Probe (Controller_HID, Result);
         when 3 => R3.Probe (Controller_HID, Result);
      end case;
      if Result.Status /= T.Probed then return; end if;
      GPIO_Native_Clock.Read (Now, Clock_OK);
      if not Clock_OK then Result.Status := T.Clock_Refused; return; end if;
      if Now >= Deadline then Result.Status := T.Expired; return; end if;
      Resources (Resource).Probed := True;
   end Probe;
   procedure Freeze (Ready_Resources : out Resource_Count) is
   begin
      Frozen := True; Ready_Resources := 0;
      for Resource of Resources loop
         if Resource.Probed then Ready_Resources := Ready_Resources + 1; end if;
      end loop;
   end Freeze;
   procedure Admit_Pin (Pin : G.ACPI_Pin; Result : out T.Pin_Admission; Plan : out G.Plan_Result) is
      Where : constant G.Location := G.Locate (Pin);
   begin
      Plan := (Status => G.No_GPIO_Pin); Result := T.Header_Unavailable;
      if not Where.Found then Result := T.Unsupported_Pin; return; end if;
      if not Ready (Where.Resource) then return; end if;
      case Where.Resource is
         when 0 => R0.Admit_Pin (Pin, Result, Plan);
         when 1 => R1.Admit_Pin (Pin, Result, Plan);
         when 2 => R2.Admit_Pin (Pin, Result, Plan);
         when 3 => R3.Admit_Pin (Pin, Result, Plan);
      end case;
   end Admit_Pin;
   procedure Read
     (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin
      Value := 0; Status := I.Unavailable;
      if Plan.Status /= G.Ready or else not Ready (Plan.Selected.Resource) then return; end if;
      case Plan.Selected.Resource is
         when 0 => R0.Read (Plan, ID, Value, Status);
         when 1 => R1.Read (Plan, ID, Value, Status);
         when 2 => R2.Read (Plan, ID, Value, Status);
         when 3 => R3.Read (Plan, ID, Value, Status);
      end case;
   end Read;
   procedure Write
     (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
   begin
      Status := I.Unavailable;
      if Plan.Status /= G.Ready or else not Ready (Plan.Selected.Resource) then return; end if;
      case Plan.Selected.Resource is
         when 0 => R0.Write (Plan, ID, Value, Status);
         when 1 => R1.Write (Plan, ID, Value, Status);
         when 2 => R2.Write (Plan, ID, Value, Status);
         when 3 => R3.Write (Plan, ID, Value, Status);
      end case;
   end Write;
end GPIO_Native_Communities;
