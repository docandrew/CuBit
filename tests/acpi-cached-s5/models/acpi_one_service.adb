pragma Ada_2022;
package body ACPI_One_Service is
   package body Root is
      package body Hardware is
         procedure Sleep_Action (Command : ACPI_Sleep_Entry.Command; Deadline_Microseconds : Interfaces.Unsigned_64;
            Output : out ACPI_Sleep_Entry.Control_Word; Result : out ACPI_Sleep_Entry.Completion) is
            use type Interfaces.Unsigned_64;
         begin
            if Deadline_Microseconds <= S5_Boundary.Microseconds or else not Admitting then raise Program_Error; end if;
            S5_Boundary.Commands := S5_Boundary.Commands + 1;
            S5_Boundary.Issued (S5_Boundary.Commands) := Command;
            Output := S5_Boundary.Saved_Read; Result := S5_Boundary.Hardware_Result;
         end Sleep_Action;
      end Hardware;
   end Root;
end ACPI_One_Service;
