pragma Ada_2022;
-- Process exit is distinct from successful AML _REG disconnect/retirement.
-- Evidence is supplied by trusted serialized local owners, never a wire flag.
package ACPI_Region_Stop_Policy with SPARK_Mode, Pure is
   type Requirement is (Authenticated_Stop, Region_State_Retained,
      Shutdown_Closed, SCI_Quiesced, Runtime_Stopped, Fields_Quiet,
      EC_Quiet, Hardware_Quiet, Backing_Quiet, Other_Actors_Quiet,
      Region_Work_Quiet);
   type Evidence is array (Requirement) of Boolean;
   function Allowed (Facts : Evidence) return Boolean is
     (for all Item in Requirement => Facts (Item)) with Global => null;
   type Refusal (Missing : Boolean := False) is record
      case Missing is when True => Item : Requirement; when False => null; end case;
   end record;
   function First_Missing (Facts : Evidence) return Refusal with Global => null,
      Post => (First_Missing'Result.Missing = not Allowed (Facts))
        and then (if First_Missing'Result.Missing then not Facts (First_Missing'Result.Item));
end ACPI_Region_Stop_Policy;
