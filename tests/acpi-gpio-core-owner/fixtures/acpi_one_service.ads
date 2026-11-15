with Prep_Model;with ACPI_Service;
package ACPI_One_Service is
 package Root is
 package Fields is
 type Field_Phase is(Owner_Ready,Owner_Failed);
 type Field_Report is record Stage:Field_Phase:=Owner_Ready;IO_Pending:Boolean:=False;end record;
 type Uncertainty is record Present:Boolean:=False;end record;
 function Control_Idle return Boolean is(not Prep_Model.Busy);
 function Observe return Field_Report is((Stage=>Owner_Ready,IO_Pending=>False));
 function Uncertain return Uncertainty is((Present=>Prep_Model.Uncertain));
 package EC_Commands is function Quiescent return Boolean is(not Prep_Model.Busy);end EC_Commands;
 end Fields;
 package Evaluation is
 procedure Evaluate_Named(Service:aliased in out ACPI_Service.State;Item:ACPI_Service.Node_Descriptor;Budget:Natural;
 Result:out ACPI_Service.Values.Result;Status:out ACPI_Service.Values.Access_Status) renames ACPI_Service.Evaluate_Named;
 end Evaluation;
 end Root;
 package Runtime is package Status is type Phase is(Fresh,Active,Stopped);end Status;
 function Current return Status.Phase is(Status.Fresh);end Runtime;
end ACPI_One_Service;
