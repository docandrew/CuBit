pragma Ada_2022;
package body ACPI_Local_Memory.Modules is
   use type Hardware.Phase;
   procedure Configure (Input : CuBit.ACPI_Windows.Grant;
      Hardware_Deadline_Microseconds : Hardware.B.Tick;
      Pool : Pool_Configuration; PCI : PCI_Configuration;
      Symbol : AML_Field_Access.Endpoint_ID; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Configured or else Consumed then return; end if;
      Configured := True;
      Saved_Grant := Input; Saved_Deadline := Hardware_Deadline_Microseconds;
      Saved_Pool := Pool; Saved_PCI := PCI; Saved_Symbol := Symbol;
      Accepted := True;
   end Configure;
   procedure Initialize_Local (Facts : ACPI_FADT.Description; Accepted : out Boolean) is
      Hardware_Result : Hardware.Report;
   begin
      Accepted := False;
      if not Configured or else Consumed then return; end if;
      Consumed := True;
      Hardware.Initialize (Saved_Grant,Facts,Saved_Deadline,Hardware_Result);
      if Hardware_Result.Stage /= Hardware.Ready then return; end if;
      ACPI_Local_Memory.Initialize (Saved_Pool,Saved_PCI,Accepted);
      if not Accepted then return; end if;
      Fields.Begin_Startup (Saved_Symbol,Accepted);
   end Initialize_Local;
   procedure Close_Local is
   begin
      Consumed := True;
      Fields.Close;
      ACPI_Local_Memory.Close_Admission;
      -- No release/cleanup attempt inside a failed Field transaction.
      -- Root drives Hardware.Begin_Stop/Maintain and Fields.Cleanup later.
   end Close_Local;
end ACPI_Local_Memory.Modules;
