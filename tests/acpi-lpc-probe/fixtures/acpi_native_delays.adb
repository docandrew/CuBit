with Probe_Model;package body ACPI_Native_Delays is
 use type Probe_Model.Test_Mode;
 procedure Inspect_Window(Deadline:out Interfaces.Unsigned_64;Available:out Boolean) is
 begin Deadline:=1000;Available:=Probe_Model.Selected/=Probe_Model.No_Clock;
 if Probe_Model.Selected=Probe_Model.No_Deadline then Deadline:=0;
 elsif Probe_Model.Selected=Probe_Model.Nonfinite then Deadline:=Interfaces.Unsigned_64'Last;end if;end;
end ACPI_Native_Delays;