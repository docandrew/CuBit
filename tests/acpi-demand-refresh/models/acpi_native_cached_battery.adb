with Interfaces;with Demand_Test_Control;with ACPI_Battery_Observation_Protocol;with ACPI_Cached_Battery_Codec;with ACPI_Protocol;with ACPI_Endpoint;
package body ACPI_Native_Cached_Battery is
 package T renames Demand_Test_Control;package M renames CuBit.Messages;
 package P renames ACPI_Battery_Observation_Protocol;package C is new ACPI_Cached_Battery_Codec(P.Read_Metrics_Label,P.Battery_Page);
 use type C.V.Generation,C.Decision,Interfaces.Unsigned_64,P.Subpage;
 procedure Dispatch(Server:ACPI_Requests.State;Request:M.Message;Read_Allowed:Boolean;Reply:out M.Message;Handled:out Boolean) is
 pragma Unreferenced(Server);
 Selected:constant C.Selection:=C.Inspect((Request.tag.label,Request.tag.length,M.Flag_Byte(Request.tag),Request.tag.reserved,C.Words(Request.words)),ACPI_Protocol.Role_Tag(Request.authorityTag));
 begin
 Reply:=M.NULL_MESSAGE;Reply.tag.length:=4;Handled:=Selected.Kind/=C.Not_Handled;if not Handled then return;end if;
 if Selected.Kind/=C.Read_Inventory then Reply.tag.label:=ACPI_Endpoint.Reply_Error;Reply.words(0):=1;return;end if;
 Reply.tag.label:=ACPI_Endpoint.Reply_OK;
 if not Read_Allowed then
  if Selected.Expected>0 then Reply.tag.label:=ACPI_Endpoint.Reply_Error;Reply.words(0):=3;end if;
  return;
 end if;
 T.Read_Calls:=T.Read_Calls+1;
 if Selected.Expected/=0 and then Interfaces.Unsigned_64(Selected.Expected)/=T.Generation then Reply.tag.label:=ACPI_Endpoint.Reply_Error;Reply.words(0):=3;return;end if;
 Reply.words:=[T.Generation,T.Cache_Status,0,0];if Selected.Page=P.Sample_Time then Reply.words(1):=1;end if;
 end;
end ACPI_Native_Cached_Battery;
