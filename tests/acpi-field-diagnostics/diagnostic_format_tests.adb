pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Field_Diagnostics;
procedure Diagnostic_Format_Tests is
   package D renames ACPI_Field_Diagnostics;
   use type D.Report;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   First : D.Report;
begin
   Check (D.Format (First) = "cause=UNCLASSIFIED at=NONE");
   for Reason in D.Cause loop
      declare
         Saved : constant D.Report := (Reason => Reason, At_Point => D.Read_Reply, others => <>);
         Other : constant D.Report := (Reason => D.Backend_Late, At_Point => D.End_Lease, others => <>);
      begin
         First := D.No_Report; D.Remember (First, Saved); Check (First = Saved);
         D.Remember (First, Other);
         Check (First = (if D.Present (Saved) then Saved else Other));
         Check (D.Name (Reason)'Length > 0);
      end;
   end loop;
   for Step in D.Point loop Check (D.Name (Step)'Length > 0); end loop;
   Check (D.Format ((Reason => D.Lease_Deadline, At_Point => D.End_Lease, others => <>)) =
     "cause=LEASE_DEADLINE at=FIELD_END");
   Check (D.Elapsed_Milliseconds (0, 0, True) = " 0");
   Check (D.Elapsed_Milliseconds (1_000, 3_999, True) = " 2");
   Check (D.Elapsed_Milliseconds (2_000, 1_999, True) = "UNKNOWN");
   Check (D.Elapsed_Milliseconds (0, 3_999, False) = "UNKNOWN");
   Check (D.Elapsed_Milliseconds (D.Microseconds'Last, D.Microseconds'Last, True) = " 0");
   Ada.Text_IO.Put_Line ("Field diagnostic formatting checks" & Checks'Image);
end Diagnostic_Format_Tests;
