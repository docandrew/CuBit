pragma Ada_2022;
with AML_Decode;
package body ACPI_Region_Initialization.Descriptor_Tests is
   procedure Run (Checks : out Natural) is
      package N renames Core.Namespace;
      use type N.Region_Space_ID;
      Saved : N.Operation_Region_Attributes :=
        (Space => 2, Address => 16#10#, Length => 16#100#, others => <>);
      Resolved : N.Operation_Region_Attributes := Saved;
      Different : N.Operation_Region_Attributes;
      procedure Check (Good : Boolean) is
      begin
         Checks := Checks + 1;
         if not Good then raise Program_Error with "descriptor check" & Checks'Image; end if;
      end Check;
   begin
      Checks := 0;
      Check (Same_Declaration (Saved, Saved));
      Resolved.PCI := (Known => True, Device => (Segment => 0, Bus => 0, Device => 1, Func => 0));
      Check (Same_Declaration (Resolved, Saved));
      Check (Same_Declaration (Resolved, Resolved));
      Check (not Same_Declaration (Saved, Resolved));
      Different := Resolved;
      Different.PCI.Device.Bus := 1;
      Check (not Same_Declaration (Different, Resolved));
      Different := Resolved; Different.PCI.Device.Segment := 1;
      Check (not Same_Declaration (Different, Resolved));
      Different := Resolved; Different.PCI.Device.Device := 2;
      Check (not Same_Declaration (Different, Resolved));
      Different := Resolved; Different.PCI.Device.Func := 1;
      Check (not Same_Declaration (Different, Resolved));
      for Space in N.Region_Space_ID loop
         Different := Resolved; Different.Space := Space;
         if Space = 2 then Check (Same_Declaration (Different, Saved));
         else Check (not Same_Declaration (Different, Saved)); end if;
      end loop;
      Different := Resolved; Different.Address := 17;
      Check (not Same_Declaration (Different, Saved));
      Different := Resolved; Different.Length := 257;
      Check (not Same_Declaration (Different, Saved));
      Different := Resolved; Different.Width := AML_Decode.Bits_32;
      Check (not Same_Declaration (Different, Saved));
      Different := Resolved; Different.Syntax.Address_Is_Literal := True;
      Check (not Same_Declaration (Different, Saved));
      Different := Resolved; Different.Syntax.Length_Is_Literal := True;
      Check (not Same_Declaration (Different, Saved));
      Different := Resolved;
      Different.Origin := (Known => True, Table => 1, Definition => N.No_Definition);
      Check (not Same_Declaration (Different, Saved));
   end Run;
end ACPI_Region_Initialization.Descriptor_Tests;
