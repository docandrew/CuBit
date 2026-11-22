with Probe_Model;package body ACPI_Native_Log is
 use type Probe_Model.Test_Mode;
 procedure Copied(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False) is pragma Unreferenced(Success);
 begin if Peer_Quiet then raise Program_Error with "probe may not open logging";end if;
 Probe_Model.Logs:=Probe_Model.Logs+1;
 if Probe_Model.Selected=Probe_Model.Shed then Probe_Model.Dropped:=Probe_Model.Dropped+1;return;end if;
 Probe_Model.Last_Text(Probe_Model.Last+1..Probe_Model.Last+Text'Length):=Text;Probe_Model.Last:=Probe_Model.Last+Text'Length;end;
end ACPI_Native_Log;