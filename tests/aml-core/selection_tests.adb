pragma Ada_2022;
with Ada.Text_IO;
with AML_Table_Backing; use AML_Table_Backing;
with Firmware_Tables.Identifiers;
procedure Selection_Tests is
   Input : aliased State (3, 128);
   Query : Firmware_Tables.Identifiers.Selection := (Name => "TEST", others => <>);
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Put (Offset : Natural; Text : String) is
   begin
      for I in Text'Range loop
         Input.Data (Offset + I - Text'First + 1) := Character'Pos (Text (I));
      end loop;
   end Put;
begin
   Check (Find_Table (Input, Query) = 0);
   Input.Count := 2;
   Input.Tables (1) := (Offset => 1, Extent => 36);
   Input.Tables (2) := (Offset => 53, Extent => 36);
   Put (1, "TEST"); Put (11, "FIRST "); Put (17, "TABLE001");
   Put (53, "TEST"); Put (63, "SECOND"); Put (69, "TABLE002");
   Check (Find_Table (Input, Query) = 1);
   Query.Match_OEM := True; Query.OEM := "SECOND";
   Check (Find_Table (Input, Query) = 2);
   Query.Match_OEM_Table := True; Query.OEM_Table := "TABLE002";
   Check (Find_Table (Input, Query) = 2);
   Query.OEM_Table := "TABLE001";
   Check (Find_Table (Input, Query) = 0);
   Query.Match_OEM := False;
   Check (Find_Table (Input, Query) = 1);
   Query := (Name => "NONE", others => <>);
   Check (Find_Table (Input, Query) = 0);
   Query.Name := "TEST";
   -- Invalid later span invalidates the inventory, even if entry one matches.
   Input.Tables (2).Offset := Natural'Last;
   Check (Find_Table (Input, Query) = 0);
   Input.Tables (2) := (Offset => 53, Extent => 36);
   Input.Count := 4;
   Check (Find_Table (Input, Query) = 0);
   Check (not Matches_Table (Input, 1, Query));
   Input.Count := 2;
   Check (not Matches_Table (Input, Positive'Last, Query));
   for Offset in 0 .. 129 loop
      for Extent in 0 .. 129 loop
         Input.Tables (2) := (Offset => Offset, Extent => Extent);
         declare
            Valid : constant Boolean := Extent >= 36 and then Offset <= 128
              and then Extent <= 128 - Offset;
         begin
            Check (Valid_Span (Input, 2) = Valid);
            Check (Find_Table (Input, Query) = (if Valid then 1 else 0));
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("ACPI owned table selection checks:" & Checks'Image);
end Selection_Tests;
