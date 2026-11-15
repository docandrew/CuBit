with Probe_Model;package body ACPI_One_Service is
 use type Probe_Model.Test_Mode;
 package body Root is package body Fields is
 function Control_Idle return Boolean is(Probe_Model.Selected/=Probe_Model.Unsafe);
 function Uncertain return Observation is((Present=>False));
 package body EC_Commands is function Quiescent return Boolean is(True);end EC_Commands;
 end Fields;end Root;end ACPI_One_Service;