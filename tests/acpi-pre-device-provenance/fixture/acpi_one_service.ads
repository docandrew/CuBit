-- Hosted test model only; never a native runtime/provider implementation.
with Prep_Model; package ACPI_One_Service is
 package Root is package Fields is
 type Field_Report is record IO_Pending : Boolean:=False; end record;
 type Uncertainty is record Present : Boolean:=False; end record;
 function Control_Idle return Boolean is (not Prep_Model.Busy);
 function Observe return Field_Report is ((IO_Pending=>False));
 function Uncertain return Uncertainty is ((Present=>Prep_Model.Uncertain));
 package EC_Commands is function Quiescent return Boolean is (not Prep_Model.Busy); end EC_Commands;
 end Fields; end Root;
 package Runtime is package Status is type Phase is (Fresh,Active,Stopped); end Status;
 function Current return Status.Phase is (Status.Fresh); end Runtime;
end ACPI_One_Service;