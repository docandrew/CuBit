pragma Ada_2022;
with ACPI_Service;
with ACPI_One_Service;
with ACPI_Clock_Source;
with ACPI_Lid;with ACPI_Power_Source;
with ACPI_Requests.Lids;with ACPI_Requests.Lids.With_Local_Events;
with ACPI_Requests.Power_Sources;with ACPI_Requests.Power_Sources.With_Local_Events;
with ACPI_Battery;with ACPI_Battery_Information;with ACPI_Battery_Information_Reader;
with ACPI_Battery_Legacy_Information;with ACPI_Battery_Legacy_Information_Reader;
with ACPI_Battery_Information_Selection;with ACPI_Battery_Observation;with ACPI_Battery_Inventory;
with ACPI_Requests.Battery_Observations;with ACPI_Requests.Battery_Observations.With_Local_Events;
with ACPI_HID_I2C_Candidate;with ACPI_HID_I2C_Discovery;with ACPI_HID_I2C_Resolved;with ACPI_HID_Inventory;
with ACPI_Requests.HID_Devices;with ACPI_Requests.HID_Devices.With_Local_Events;
with ACPI_Local_Sample_Status;with Interfaces;
package body ACPI_Native_Consumers is
   package C renames ACPI_Service;package Q renames ACPI_Requests;package One renames ACPI_One_Service;
   package Status renames ACPI_Local_Sample_Status;
   package LD is new ACPI_Lid(C,One.Runtime.Evaluate_Named);
   package PD is new ACPI_Power_Source(C,One.Runtime.Evaluate_Named);
   package Lids is new Q.Lids(LD);package Sources is new Q.Power_Sources(PD);
   package Format is new ACPI_Battery_Information;
   package Dynamic is new ACPI_Battery(C,One.Runtime.Evaluate_Named);
   package Extended_Info is new ACPI_Battery_Information_Reader(C,Format,One.Runtime.Evaluate_Named);
   package Legacy_Format is new ACPI_Battery_Legacy_Information;
   package Legacy_Info is new ACPI_Battery_Legacy_Information_Reader(C,Legacy_Format,One.Runtime.Evaluate_Named);
   package Static_Info is new ACPI_Battery_Information_Selection(C,Format,Legacy_Format,Extended_Info,Legacy_Info);
   package Collector is new ACPI_Battery_Observation(C,Format,Dynamic,Static_Info);
   package Batteries is new Q.Battery_Observations(Dynamic,Collector);
   Maximum_Batteries : constant Positive := Maximum_Battery_Facts;
   package BI is new ACPI_Battery_Inventory(Collector,Maximum_Batteries);
   Maximum_Devices : constant Positive := Maximum_HID_Facts;
   Maximum_Source_Bytes : constant Positive := 256;
   Maximum_Resource_Bytes : constant Positive := 65_536;
   package Candidates is new ACPI_HID_I2C_Candidate(Maximum_Source_Bytes);
   package Discovery is new ACPI_HID_I2C_Discovery(C,One.Runtime.Invoke_With_Arguments,One.Runtime.Evaluate_Named);
   package Resolver is new ACPI_HID_I2C_Resolved(C,Candidates);
   package Hids is new Q.HID_Devices(Discovery,Resolver);
   package HI is new ACPI_HID_Inventory(Resolver,Maximum_Devices);
   use type One.Runtime.Status.Phase,Status.Outcome,Status.Notice;
   use type Lids.Samples.Availability,Sources.Samples.Availability,BI.Samples.Availability,HI.Samples.Availability;
   use type BI.Availability;
   Fuel : constant Positive := 100_000;
   Window : constant Status.Window_Allowance := 500_000;
   Last : Report;
   Attempted,Working : Boolean := False;
   Pending,Runnable : array(Family) of Boolean := [others=>False];
   Next_Family : Family := Lid;
   Lid_Value:Lids.Samples.View;Source_Value:Sources.Samples.View;
   Battery_Value:BI.Samples.View;HID_Value:HI.Samples.View;
   Kinds:array(Family) of Status.Notice:=[others=>Status.No_Device];
   Battery_Cursor:BI.Slot:=BI.Slot'First;HID_Cursor:HI.Slot:=HI.Slot'First;
   function Observe return Report is (Last);
   function Facts_Pending return Boolean is (for some F in Family=>Pending(F));
   function Healthy return Boolean is
     (Last.Why=None and then Sources_Healthy);
   function Owner_Quiet(Server:Q.State) return Boolean is
     (not Q.Notifications_Pending(Server) and then not Q.Retained_Results_Pending(Server)
      and then One.Runtime.Current not in One.Runtime.Status.Fresh|One.Runtime.Status.Active
      and then One.Root.Fields.Control_Idle);
   procedure Capture(F:Family;Kind:Status.Notice;Accepted:out Boolean) is
   begin
      Accepted:=False;
      if Pending(F) then Last.Why:=Publication_Overlap;Last.At_Family:=F;return;end if;
      Pending(F):=True;Kinds(F):=Kind;Accepted:=True;
   end Capture;
   procedure Read_Lid_Clock(Now:out Lids.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=Lids.Samples.Tick(Value);end Read_Lid_Clock;
   procedure Read_Source_Clock(Now:out Sources.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=Sources.Samples.Tick(Value);end Read_Source_Clock;
   procedure Read_Battery_Clock(Now:out BI.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=BI.Samples.Tick(Value);end Read_Battery_Clock;
   procedure Read_HID_Clock(Now:out HI.Samples.Tick;Available:out Boolean) is
      Value:Interfaces.Unsigned_64;
   begin ACPI_Clock_Source.Read(Value,Available);Now:=HI.Samples.Tick(Value);end Read_HID_Clock;
   -- Fast copied-fact sinks only. No IPC, rendering, Core mutation or callback.
   procedure Report_Lid(Server:Q.State;Kind:Status.Notice;Value:Lids.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin Capture(Lid,Kind,Accepted);if Accepted then Lid_Value:=Value;end if;end Report_Lid;
   procedure Report_Source(Server:Q.State;Kind:Status.Notice;Value:Sources.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin Capture(AC_Source,Kind,Accepted);if Accepted then Source_Value:=Value;end if;end Report_Source;
   procedure Report_Battery(Server:Q.State;Kind:Status.Notice;Value:BI.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin Capture(Battery,Kind,Accepted);if Accepted then Battery_Value:=Value;Battery_Cursor:=BI.Slot'First;end if;end Report_Battery;
   procedure Report_HID(Server:Q.State;Kind:Status.Notice;Value:HI.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Server,Flags);Accepted:Boolean;
   begin Capture(HID_I2C,Kind,Accepted);if Accepted then HID_Value:=Value;HID_Cursor:=HI.Slot'First;end if;end Report_HID;
   package LE is new Lids.With_Local_Events(One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_Lid_Clock,Report_Lid,Healthy,Fuel,Window);
   package PE is new Sources.With_Local_Events(One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_Source_Clock,Report_Source,Healthy,Fuel,Window);
   package BE is new Batteries.With_Local_Events(BI,One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_Battery_Clock,Report_Battery,Healthy,Fuel,Window);
   package HE is new Hids.With_Local_Events(HI,One.Runtime.Current,One.Runtime_Access.Begin_Event,One.Runtime_Access.End_Event,
      Read_HID_Clock,Report_HID,Healthy,Maximum_Resource_Bytes,Fuel,Window);
   procedure Start(Server:Q.State;Accepted:out Boolean) is
      L,P,B,H:Boolean;
   begin
      Accepted:=False;if Attempted or Working or Last.Stopping then return;end if;Attempted:=True;
      if Q.Retained_Results_Pending(Server) or else not One.Root.Fields.Control_Idle
        or else One.Runtime.Current/=One.Runtime.Status.Idle or else not Sources_Healthy then Last.Why:=Not_Ready;return;end if;
      Working:=True;LE.Start(Server,L);PE.Start(Server,P);BE.Start(Server,B);HE.Start(Server,H);
      Last.Started:=L and P and B and H;Accepted:=Last.Started;
      if not Accepted then Last.Why:=Start_Refused;else Runnable:=[others=>True];end if;
      Working:=False;
   end Start;
   procedure Route(Server:Q.State;Routed:out Boolean) is
      L,P,B,H:Boolean;
   begin
      Routed:=False;if Working or not Last.Started or Last.Closed then return;end if;
      -- Every route is attempted, including after an earlier refusal. ACK is
      -- authorized only by their conjunction in the caller's private broker.
      LE.Route(Server,L);PE.Route(Server,P);BE.Route(Server,B);HE.Route(Server,H);
      Routed:=L and P and B and H;
      if Routed and not Last.Stopping then Runnable:=[others=>True];end if;
   end Route;
   procedure Advance(Server:in out Q.State;Result:out ACPI_Native_Server.Owner_Outcome) is
      Outcome:Status.Outcome;F:Family:=Next_Family;Found:Boolean:=False;
   begin
      Result:=ACPI_Native_Server.Owner_Refused;
      if Working or not Last.Started or Last.Why/=None then return;end if;
      if Last.Stopping then Result:=ACPI_Native_Server.Owner_Idle;return;end if;
      if not Sources_Healthy then Last.Why:=Source_Unhealthy;return;end if;
      if Q.Notifications_Pending(Server) or Facts_Pending then Result:=ACPI_Native_Server.Owner_Pending;return;end if;
      if not Owner_Quiet(Server) or One.Runtime.Current/=One.Runtime.Status.Idle then Last.Why:=Not_Ready;return;end if;
      for Step in Family loop
         if Runnable(F) then Found:=True;exit;end if;
         F:=(if F=Family'Last then Family'First else Family'Succ(F));
      end loop;
      if not Found then Result:=ACPI_Native_Server.Owner_Idle;return;end if;
      Working:=True;Last.At_Family:=F;
      case F is
         when Lid=>LE.Advance(Server,Outcome);
         when AC_Source=>PE.Advance(Server,Outcome);
         when Battery=>BE.Advance(Server,Outcome);
         when HID_I2C=>HE.Advance(Server,Outcome);
      end case;
      Working:=False;Next_Family:=(if F=Family'Last then Family'First else Family'Succ(F));
      if Last.Why/=None then return;end if;
      if Outcome=Status.Refused then Last.Why:=Family_Refused;return;end if;
      Runnable(F):=Outcome=Status.Pending;
      Result:=(if Facts_Pending or else (for some Kind in Family=>Runnable(Kind))
         then ACPI_Native_Server.Owner_Pending else ACPI_Native_Server.Owner_Idle);
   end Advance;
   function Image(N:Interfaces.Unsigned_64) return String is
      Text:constant String:=N'Image;
   begin return Text(Text'First+1..Text'Last);end Image;
   procedure Take_Fact(Server:Q.State;Line:out Fact_Line;Last:out Fact_Length;Present:out Boolean) is
      More:Boolean:=False;
      procedure Emit(Text:String) is
      begin
         if Text'Length>Line'Length then ACPI_Native_Consumers.Last.Why:=Fact_Too_Long;return;end if;
         Last:=Text'Length;Line(1..Last):=Text;Present:=True;
      end Emit;
   begin
      Line:=[others=>' '];Last:=0;Present:=False;
      if Working or not Owner_Quiet(Server) then return;end if;
      for F in Family loop
         if Pending(F) then
            case F is
               when Lid=>
                  if Kinds(F)=Status.Sample_Available and Lid_Value.Status=Lids.Samples.Available then Emit("acpi: lid "&Lid_Value.Value.State'Image);
                  else Emit("acpi: lid "&Kinds(F)'Image);end if;
               when AC_Source=>
                  if Kinds(F)=Status.Sample_Available and Source_Value.Status=Sources.Samples.Available then Emit("acpi: AC "&Source_Value.Value.State'Image);
                  else Emit("acpi: AC "&Kinds(F)'Image);end if;
               when Battery=>
                  if Kinds(F)=Status.Sample_Available and Battery_Value.Status=BI.Samples.Available and Battery_Value.Value.Count>0 then
                     declare Item:BI.Entry_Value renames Battery_Value.Value.Items(Battery_Cursor);begin
                        if Item.Status=BI.Sampled then
                           declare Info:constant Static_Info.Common_Information:=Static_Info.Common(Item.Value.Information);begin
                              Emit("acpi: battery "&Image(Interfaces.Unsigned_64(Battery_Cursor))&" "&Item.Value.Status.Flow'Image&
                                 " remaining="&(if Item.Value.Status.Remaining_Capacity.Known then Image(Interfaces.Unsigned_64(Item.Value.Status.Remaining_Capacity.Value)) else "UNKNOWN")&
                                 " full="&(if Info.Last_Full.Known then Image(Interfaces.Unsigned_64(Info.Last_Full.Value)) else "UNKNOWN")&" units="&Info.Units'Image);
                           end;
                        else Emit("acpi: battery "&Image(Interfaces.Unsigned_64(Battery_Cursor))&" "&Item.Status'Image&" reason="&Item.Reason'Image);end if;
                     end;
                     More:=Natural(Battery_Cursor)<Natural(Battery_Value.Value.Count);
                     if Present and More then Battery_Cursor:=Battery_Cursor+1;end if;
                  else Emit("acpi: battery "&Kinds(F)'Image&" count="&Image(Interfaces.Unsigned_64(Battery_Value.Value.Count)));end if;
               when HID_I2C=>
                  if Kinds(F)=Status.Sample_Available and HID_Value.Status=HI.Samples.Available and HID_Value.Value.Count>0 then
                     declare Item:constant Candidates.Candidate:=Resolver.Facts(HID_Value.Value.Items(HID_Cursor));begin
                        Emit("acpi: HID-I2C candidate "&Image(Interfaces.Unsigned_64(HID_Cursor))&" addr="&Image(Interfaces.Unsigned_64(Item.I2C.Address))&
                           " descriptor="&Image(Interfaces.Unsigned_64(Item.Descriptor_Register))&" gpio="&Image(Interfaces.Unsigned_64(Item.Pin)));
                     end;
                     More:=Natural(HID_Cursor)<Natural(HID_Value.Value.Count);
                     if Present and More then HID_Cursor:=HID_Cursor+1;end if;
                  else Emit("acpi: HID-I2C "&Kinds(F)'Image&" count="&Image(Interfaces.Unsigned_64(HID_Value.Value.Count)));end if;
            end case;
            if Present and not More then Pending(F):=False;end if;return;
         end if;
      end loop;
   end Take_Fact;
   procedure Begin_Stop is begin Last.Stopping:=True;end Begin_Stop;
   function Quiet(Server:Q.State) return Boolean is
     (Last.Closed and then not Working and then not Facts_Pending and then Owner_Quiet(Server));
   procedure Close(Server:Q.State;Completed:out Boolean) is
   begin
      Completed:=False;Begin_Stop;
      if Working or Facts_Pending or not Owner_Quiet(Server) then return;end if;
      LE.Stop;PE.Stop;BE.Stop;HE.Stop;Last.Closed:=True;Completed:=Quiet(Server);
   end Close;
end ACPI_Native_Consumers;
