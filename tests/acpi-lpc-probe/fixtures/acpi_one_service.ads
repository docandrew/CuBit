with Interfaces;package ACPI_One_Service is
 Field_Scope:constant Interfaces.Unsigned_64:=16#ABCD_0000_0000_0001#;
 package Root is package Fields is
 function Control_Idle return Boolean;
 type Observation is record Present:Boolean:=False;end record;
 function Uncertain return Observation;
 package EC_Commands is function Quiescent return Boolean;end EC_Commands;
 end Fields;end Root;
end ACPI_One_Service;