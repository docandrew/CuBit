with Prep_Model;
package body ACPI_Native_Initializer_History is
 function Observe return Report is
 begin return (Invocations=>(if Prep_Model.Mode=25 and Prep_Model.Copies>0 then 4 else 3),Complete=>Prep_Model.Mode/=24,others=><>);end;
end ACPI_Native_Initializer_History;
