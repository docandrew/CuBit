with ACPI_Delay_Domains;
package body ACPI_One_Service is
   package C renames ACPI_Service;package D renames ACPI_Delay_Domains;
   use type C.Context_Key,Runtime.Outcome;
   Anchor:C.Context_Key:=C.No_Context;Domain:D.State;
   Claimed,Closed:Boolean:=False;
   Clock:constant Interfaces.Unsigned_64:=20;
   function Ready(Service:C.State) return Boolean is
     (not Closed and then Owner_Allowed and then C.Current_Context(Service,Anchor));
   function Idle return Boolean is(True);
   procedure Claim(Service:C.State;Good:out Boolean) is
   begin Good:=Ready(Service) and not Claimed;if Good then Claimed:=True;end if;end Claim;
   procedure Close_Owner is begin Closed:=True;end Close_Owner;
   procedure Enter_Delays(Good:out Boolean) is
   begin D.Enter_Runtime(Domain,Clock,True,Good);end Enter_Delays;
   procedure Begin_Delays(Deadline:Interfaces.Unsigned_64;Good:out Boolean) is
   begin D.Begin_Event(Domain,Clock,Deadline,True,Good);end Begin_Delays;
   procedure End_Delays(Good:out Boolean) is
   begin if Close_Allowed then D.End_Event(Domain,Clock,True,Good);else Good:=False;end if;end End_Delays;
   procedure Stop_Delays is begin D.Stop(Domain);end Stop_Delays;
   procedure Attach(Server:ACPI_Requests.State;Accepted:out Boolean) is
      procedure Bind(Service:C.State;Completed:out Boolean) is
      begin Anchor:=C.Capture_Context(Service);Completed:=Anchor/=C.No_Context;end Bind;
      package Metadata is new ACPI_Requests.With_Metadata_Continuation(Bind);
      Result:Runtime.Outcome;
   begin
      Metadata.Advance(Server,Accepted);if not Accepted then return;end if;
      D.Start(Domain,10,1000,True,Accepted);if not Accepted then return;end if;
      Runtime_Access.Enter_Runtime(Server,Result);Accepted:=Result=Runtime.Status.Runtime_Entered;
   end Attach;
   package body Root is
      package body Hardware is function Admitting return Boolean is(Owner_Allowed);end Hardware;
      package body Fields is
         function Control_Idle return Boolean is(True);
         package body EC_Commands is function Quiescent return Boolean is(True);end EC_Commands;
      end Fields;
   end Root;
end ACPI_One_Service;
