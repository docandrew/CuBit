with Ada.Text_IO;
with FADT_Tests;
with ACPI_FADT;
with ACPI_Service; use ACPI_Service;
with Firmware_Tables;
with Firmware_Tables.Identifiers;
with AML_Decode;
with AML_Field_Data;
procedure Service_Tests is
   use type Firmware_Tables.Byte;
   use type Firmware_Tables.Bytes;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Status;
   use type Namespace.Bind_Status;
   use type AML_Field_Data.Read_Result;
   use type Namespace.State;
   Service : State := Fresh;
   Result : Install_Status;
   Data : Firmware_Tables.Bytes (1 .. 42) := [others => 0];
   Before : Namespace.State;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Named_Table (First, Count : Natural; Is_SSDT : Boolean := False)
     return Firmware_Tables.Bytes
   is
      Value : Firmware_Tables.Bytes (1 .. 36 + 6 * Count) := [others => 0];
      Sig : constant String := (if Is_SSDT then "SSDT" else "DSDT");
      Hex : constant String := "0123456789ABCDEF";
      Sum : Firmware_Tables.Byte := 0;
   begin
      for I in 1 .. 4 loop
         Value (I) := Character'Pos (Sig (I));
         Value (I + 4) := Firmware_Tables.Byte ((Value'Length / 256 ** (I - 1)) mod 256);
      end loop;
      Value (9) := 2;
      for I in 0 .. Count - 1 loop
         Value (37 + I * 6 .. 42 + I * 6) :=
           [8, Character'Pos ('N'), Character'Pos (Hex ((First + I) / 256 + 1)),
            Character'Pos (Hex (((First + I) / 16) mod 16 + 1)),
            Character'Pos (Hex ((First + I) mod 16 + 1)), 0];
      end loop;
      for B of Value loop Sum := Sum + B; end loop;
      Value (10) := 0 - Sum;
      return Value;
   end Named_Table;
   function Description_Table (Sig : Firmware_Tables.Signature; Extent : Positive;
                               Seed : Natural := 0) return Firmware_Tables.Bytes is
      Value : Firmware_Tables.Bytes (1 .. Extent);
      Sum : Firmware_Tables.Byte := 0;
   begin
      for I in Value'Range loop Value (I) := Firmware_Tables.Byte ((I + Seed) mod 256); end loop;
      for I in 1 .. 4 loop
         Value (I) := Character'Pos (Sig (I));
         Value (I + 4) := Firmware_Tables.Byte ((Extent / 256 ** (I - 1)) mod 256);
      end loop;
      Value (9) := 2;
      Value (10) := 0;
      for B of Value loop Sum := Sum + B; end loop;
      Value (10) := 0 - Sum;
      return Value;
   end Description_Table;
   procedure Seal (Kind : Table_Kind; Revision : Firmware_Tables.Byte) is
      Sig : constant String := (if Kind = DSDT then "DSDT" else "SSDT");
      Sum : Firmware_Tables.Byte := 0;
   begin
      for I in 1 .. 4 loop Data (I) := Character'Pos (Sig (I)); end loop;
      Data (5) := 42;
      Data (9) := Revision;
      Data (10) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
   end Seal;
begin
   Data (37 .. 42) := [8,16#54#,16#45#,16#53#,16#54#,16#FF#];
   Seal (SSDT, 2);
   Install (Service, 1, SSDT, Data, Result);
   Check (Result = Wrong_Order and then Observe (Service).Tables = 0);
   Seal (DSDT, 1);
   Install (Service, 1, DSDT, Data, Result);
   Check (Result = Installed and then Observe (Service).Objects = 1
          and then Observe (Service).Bytes = 42);
   Check (Observe (Service).Value_Objects = 1 and then
          Observe (Service).Value_Bytes = 0 and then Observe (Service).Package_Elements = 0);
   Check (Namespace.Integer_Data (Snapshot (Service), 1) = 16#FFFF_FFFF#);
   Before := Snapshot (Service);
   Install (Service, 2, DSDT, Data, Result);
   Check (Result = Wrong_Order and then Snapshot (Service) = Before);
   Data (41) := 16#32#;
   Seal (SSDT, 2);
   Install (Service, 1, SSDT, Data, Result);
   Check (Result = Duplicate_ID and then Snapshot (Service) = Before);
   Install (Service, 2, SSDT, Data, Result);
   Check (Result = Installed and then Observe (Service).Tables = 2);
   Check (Namespace.Integer_Data (Snapshot (Service), 2) = 16#FFFF_FFFF#);
   Before := Snapshot (Service);
   for I in Data'Range loop
      declare
         Mutant : Firmware_Tables.Bytes := Data;
      begin
         Mutant (I) := Mutant (I) + 1;
         Install (Service, 3, SSDT, Mutant, Result);
         Check (Result = Invalid_Table and then Snapshot (Service) = Before);
      end;
   end loop;
   Data (37) := 16#5B#;
   Seal (SSDT, 2);
   Install (Service, 3, SSDT, Data, Result);
   Check (Result = Invalid_AML and then Snapshot (Service) = Before
          and then Observe (Service).Tables = 2);
   Check (Observe (Service).Rejections = 46);
   declare
      Empty_Table : Firmware_Tables.Bytes (1 .. 36) := [others => 0];
      Sum : Firmware_Tables.Byte := 0;
   begin
      Empty_Table (1 .. 4) := [16#53#,16#53#,16#44#,16#54#];
      Empty_Table (5) := 36;
      Empty_Table (9) := 2;
      for B of Empty_Table loop Sum := Sum + B; end loop;
      Empty_Table (10) := 0 - Sum;
      for ID in 3 .. 32 loop
         Install (Service, ID, SSDT, Empty_Table, Result);
         Check (Result = Installed and then Observe (Service).Tables = ID);
      end loop;
      Before := Snapshot (Service);
      Install (Service, 33, SSDT, Empty_Table, Result);
      Check (Result = Table_Limit and then Snapshot (Service) = Before);
      Service := Fresh;
      Install (Service, 1, DSDT,
        Firmware_Tables.Bytes'(1 .. Max_Table_Bytes + 1 => 0), Result);
      Check (Result = Byte_Limit and then Observe (Service).Tables = 0);
      Seal (DSDT, 2);
      Data (37) := 8;
      Seal (DSDT, 2);
      Install (Service, 1, DSDT, Data & [0], Result);
      Check (Result = Invalid_Table and then Observe (Service).Tables = 0);
      Install (Service, 1, DSDT, Data, Result);
      Check (Result = Installed and then Namespace.Integer_Data
             (Snapshot (Service), 1) = AML_Decode.Integer_Value'Last);
   end;
   Service := Fresh;
   Check (Observe (Service).Last_Load_Code = 0);
   Install (Service, 1, DSDT, Named_Table (0, Max_Namespace_Nodes), Result);
   Check (Result = Installed and Observe (Service).Objects = Max_Namespace_Nodes);
   Check (Observe (Service).Last_Load_Code = Namespace.Load_Status'Pos (Namespace.Loaded) + 1);
   Before := Snapshot (Service);
   Install (Service, 2, SSDT, Named_Table (Max_Namespace_Nodes, 1, True), Result);
   Check (Result = Invalid_AML and Snapshot (Service) = Before);
   Check (Observe (Service).Last_Load_Code = Namespace.Load_Status'Pos (Namespace.Storage_Full) + 1);
   declare
      Bad : Firmware_Tables.Bytes := Named_Table (0, 1, True);
   begin
      Bad (10) := Bad (10) + 1;
      Install (Service, 2, SSDT, Bad, Result);
      Check (Result = Invalid_Table and Snapshot (Service) = Before);
      Check (Observe (Service).Last_Load_Code = Namespace.Load_Status'Pos (Namespace.Storage_Full) + 1);
   end;
   -- Retained immutable SDTs, including binary payloads that are not AML.
   Service := Fresh;
   Install (Service, 11, DSDT, Named_Table (0, 0), Result);
   Check (Result = Installed);
   Before := Snapshot (Service);
   declare
      type Signatures is array (Positive range <>) of Firmware_Tables.Signature;
      Index : Positive := 2;
      Old_Load : constant Natural := Observe (Service).Last_Load_Code;
   begin
      for Sig of Signatures'("FACP", "APIC", "MCFG", "HPET", "DMAR", "SRAT", "SLIT", "ASF!") loop
         declare
            Original : constant Firmware_Tables.Bytes := Description_Table (Sig, 36 + Index * 3, Index);
            Source : Firmware_Tables.Bytes (101 .. 100 + Original'Length) := Original;
         begin
            Install (Service, Index + 20, Description, Source, Result);
            Check (Result = Installed and then Snapshot (Service) = Before);
            Check (Table_Info (Service, Index) =
              Table_Metadata'(ID => Index + 20, Signature => Sig, Extent => Original'Length, Revision => 2));
            Source := [others => 0];
            for I in Original'Range loop Check (Table_Byte (Service, Index, I - 1) = Original (I)); end loop;
            Check (Observe (Service).Last_Load_Code = Old_Load);
         end;
         Index := Index + 1;
      end loop;
      for Sig of Signatures'("DSDT", "SSDT", "FACS") loop
         Install (Service, 100, Description, Description_Table (Sig, 40), Result);
         Check (Result = Invalid_Table and then Observe (Service).Tables = Index - 1);
      end loop;
      Install (Service, 22, Description, Description_Table ("DMAR", 40), Result);
      Check (Result = Duplicate_ID and then Observe (Service).Tables = Index - 1);
      declare
         Broken : Firmware_Tables.Bytes := Description_Table ("HPET", 40);
      begin
         Broken (40) := Broken (40) + 1;
         Install (Service, 100, Description, Broken, Result);
         Check (Result = Invalid_Table and then Observe (Service).Tables = Index - 1);
      end;
      Check (Snapshot (Service) = Before);
   end;
   -- Fill exactly the aggregate byte budget, then prove rejection doesn't
   -- corrupt either end of an earlier table or the final retained table.
   Service := Fresh;
   Install (Service, 1, DSDT, Named_Table (0, 0), Result);
   Check (Result = Installed);
   for ID in 2 .. 17 loop
      declare
         Size : constant Positive := (if ID = 17 then Max_Table_Bytes - 36 else Max_Table_Bytes);
         Value : constant Firmware_Tables.Bytes := Description_Table ("DMAR", Size, ID);
      begin
         Install (Service, ID, Description, Value, Result);
         Check (Result = Installed);
         for I in Value'Range loop Check (Table_Byte (Service, ID, I - 1) = Value (I)); end loop;
      end;
   end loop;
   Check (Observe (Service).Bytes = Max_Total_Bytes and Observe (Service).Tables = 17);
   Install (Service, 18, Description, Description_Table ("HPET", 36), Result);
   Check (Result = Byte_Limit and Observe (Service).Bytes = Max_Total_Bytes and Observe (Service).Tables = 17);
   Check (Table_Byte (Service, 1, 0) = Character'Pos ('D'));
   Check (Table_Byte (Service, 17, Max_Table_Bytes - 37) = Firmware_Tables.Byte ((Max_Table_Bytes - 36 + 17) mod 256));
   Service := Fresh;
   Install (Service, 1, DSDT, Named_Table (0, 0), Result);
   Check (Result = Installed and then not Fixed_Description (Service, 1).Valid);
   declare
      Value : constant Firmware_Tables.Bytes := Description_Table ("FACP", 276);
      Expected : constant ACPI_FADT.Result := ACPI_FADT.Decode (Value);
      use type ACPI_FADT.Result;
   begin
      Install (Service, 2, Description, Value, Result);
      Check (Result = Installed and then Expected.Valid);
      Check (Fixed_Description (Service, 2) = Expected);
   end;
   declare
      package IDs renames Firmware_Tables.Identifiers;
      use type IDs.Identity;
      S : State := Fresh;
      Query : IDs.Selection := (Name => "OEMX", others => <>);
      function Table_With_ID (Ident : IDs.Identity) return Firmware_Tables.Bytes is
         Raw : Firmware_Tables.Bytes (7 .. 42) := [others => 0];
         Sum : Firmware_Tables.Byte := 0;
      begin
         for I in 1 .. 4 loop Raw (6 + I) := Character'Pos (Ident.Name (I)); end loop;
         Raw (11) := 36; Raw (15) := 2;
         for I in 1 .. 6 loop Raw (16 + I) := Character'Pos (Ident.OEM (I)); end loop;
         for I in 1 .. 8 loop Raw (22 + I) := Character'Pos (Ident.OEM_Table (I)); end loop;
         for B of Raw loop Sum := Sum + B; end loop;
         Raw (16) := 0 - Sum;
         return Raw;
      end Table_With_ID;
      Ident : IDs.Identity := ("DSDT", "CUBIT ", "BASE0001");
   begin
      Check (Find_Table (S, Query) = 0);
      for I in 1 .. 6 loop
         case I is
            when 1 => null;
            when 2 | 5 => Ident := ("OEMX", "VEND_A", "TABLE001");
            when 3 => Ident := ("OEMX", "VEND_A", "TABLE002");
            when 4 => Ident := ("OEMX", "VEND_B", "TABLE001");
            when others => Ident := ("OEMX", [others => Character'Val (0)], [others => Character'Val (0)]);
         end case;
         Install (S, 100 + I, (if I = 1 then DSDT else Description), Table_With_ID (Ident), Result);
         Check (Result = Installed);
         Check (Table_Identity (S, I) = Ident);
      end loop;
      Check (Find_Table (S, Query) = 2);
      Query.OEM := "VEND_B"; Query.OEM_Table := "TABLE002";
      for Match_OEM in Boolean loop
         for Match_Table in Boolean loop
            Query.Match_OEM := Match_OEM; Query.Match_OEM_Table := Match_Table;
            Check (Find_Table (S, Query) =
              (if Match_OEM and Match_Table then 0 elsif Match_OEM then 4
               elsif Match_Table then 3 else 2));
         end loop;
      end loop;
      Query.OEM := "VEND_A"; Query.OEM_Table := "TABLE001";
      Check (Find_Table (S, Query) = 2); -- duplicate table at 5 must not win
      Query.OEM := [others => Character'Val (0)];
      Query.OEM_Table := [others => Character'Val (0)];
      Check (Find_Table (S, Query) = 6); -- explicit zero IDs, not wildcards
      Query.Name := "NONE"; Check (Find_Table (S, Query) = 0);
      Query := (Name => "DSDT", others => <>); Check (Find_Table (S, Query) = 1);
      -- Every byte value survives fixed-width extraction, including NUL and
      -- high-bit bytes. Extraction itself deliberately performs no admission.
      for V in 0 .. 255 loop
         declare
            Raw : constant Firmware_Tables.Bytes (17 .. 52) := [others => Firmware_Tables.Byte (V)];
            Actual : constant IDs.Identity := IDs.Read_Identity (Raw);
         begin
            Check ((for all C of Actual.Name => Character'Pos (C) = V));
            Check ((for all C of Actual.OEM => Character'Pos (C) = V));
            Check ((for all C of Actual.OEM_Table => Character'Pos (C) = V));
         end;
      end loop;
      declare
         Raw : constant Firmware_Tables.Bytes (Positive'Last - 35 .. Positive'Last) :=
           [others => Character'Pos ('Z')];
         Actual : constant IDs.Identity := IDs.Read_Identity (Raw);
      begin
         Check (Actual = IDs.Identity'("ZZZZ", "ZZZZZZ", "ZZZZZZZZ"));
      end;
   end;
   -- Field reads stay within the selected table, including when the next
   -- retained table is adjacent in the backing array. Mutating input cannot
   -- affect the service-owned copy.
   declare
      S : State := Fresh;
      Source : Firmware_Tables.Bytes := Description_Table ("TEST", 72);
      Expected : constant Firmware_Tables.Bytes := Source;
      Bits : AML_Field_Data.Read_Result;
      Wanted : AML_Decode.Byte;
   begin
      Install (S, 1, DSDT, Named_Table (0, 0), Result);
      Check (Result = Installed);
      Install (S, 2, Description, Source, Result);
      Check (Result = Installed);
      Install (S, 3, Description, Description_Table ("NEXT", 72, 40), Result);
      Check (Result = Installed);
      Source := [others => 0];
      for Offset in 0 .. Expected'Length * 8 loop
         Bits := Table_Field (S, 2, Offset, 13);
         if Offset + 13 > Expected'Length * 8 then
            Check (Bits.Status = AML_Decode.Truncated);
         else
            Check (Bits.Status = AML_Decode.Accepted and then Bits.Length = 2);
            for I in 0 .. 1 loop
               Wanted := 0;
               for J in 0 .. (if I = 0 then 7 else 4) loop
                  if (Expected (1 + (Offset + I * 8 + J) / 8) and
                    Firmware_Tables.Byte (2 ** ((Offset + I * 8 + J) mod 8))) /= 0
                  then Wanted := Wanted or AML_Decode.Byte (2 ** J); end if;
               end loop;
               Check (Bits.Content (I + 1) = Wanted);
            end loop;
         end if;
      end loop;
      Bits := Table_Field (S, 2, Expected'Length * 8, 0);
      Check (Bits.Status = AML_Decode.Accepted and then Bits.Length = 0);
      Check (Table_Field (S, 2, Natural'Last, 1).Status = AML_Decode.Truncated);
   end;
   declare
      S : State := Fresh;
      Region, Field, Node : Namespace.Node_ID;
      Bound : Namespace.Bind_Status;
      Prior : Namespace.State;
   begin
      Install (S, 1, DSDT, Named_Table (0, 0), Result);
      Check (Result = Installed);
      Install (S, 2, Description, Description_Table ("TEST", 72), Result);
      Check (Result = Installed);
      Declare_Table_Region (S, 0, "RGN0", (Name => "TEST", others => <>), Region, Bound);
      Check (Bound = Namespace.Bound and then Observe (S).Objects = 1);
      Declare_Table_Field (S, 0, "FLD0", Region, 7, 65, Field, Bound);
      Check (Bound = Namespace.Bound and then Observe (S).Objects = 2);
      Check (Read_Namespace_Field (S, Field) = Table_Field (S, 2, 7, 65));
      Prior := Snapshot (S);
      Declare_Table_Region (S, 0, "MISS", (Name => "NONE", others => <>), Node, Bound);
      Check (Bound = Namespace.Binding_Invalid and then Node = 0 and then Snapshot (S) = Prior);
      Declare_Table_Region (S, 512, "MISS", (Name => "TEST", others => <>), Node, Bound);
      Check (Bound = Namespace.Binding_Invalid and then Snapshot (S) = Prior);
      Declare_Table_Region (S, 0, "MISS", (Name => "TEST", others => <>), Node, Bound, Owner => 512);
      Check (Bound = Namespace.Binding_Invalid and then Snapshot (S) = Prior);
      Declare_Table_Field (S, 0, "BAD0", Field, 0, 8, Node, Bound);
      Check (Bound = Namespace.Binding_Invalid and then Snapshot (S) = Prior);
      Declare_Table_Field (S, 0, "BAD0", Region, 576, 1, Node, Bound);
      Check (Bound = Namespace.Binding_Invalid and then Snapshot (S) = Prior);
      Declare_Table_Field (S, 0, "FLD0", Region, 0, 8, Node, Bound);
      Check (Bound = Namespace.Binding_Duplicate and then Snapshot (S) = Prior);
      Check (Read_Namespace_Field (S, 512).Status = AML_Decode.Malformed);
      Check (Read_Namespace_Field (S, Region).Status = AML_Decode.Malformed);
      Check (Read_Namespace_Field (S, 0).Status = AML_Decode.Malformed);
      Declare_Table_Field (S, 0, "ZERO", Region, 576, 0, Node, Bound);
      Check (Bound = Namespace.Bound and then Read_Namespace_Field (S, Node).Length = 0);
   end;
   -- The admitted table sizes determine storage, independently of the old
   -- prototype limits. Header identity and fields near the end must use the
   -- retained large table even after the caller overwrites its source.
   declare
      Length : constant Positive := 1_048_577;
      S : State := Fresh (40, Length + 39 * 36, Length);
      Source : Firmware_Tables.Bytes := Description_Table ("LARG", Length, 17);
      Last_Byte : constant Firmware_Tables.Byte := Source (Source'Last);
      Region, Field : Namespace.Node_ID;
      Bound : Namespace.Bind_Status;
      Bits : AML_Field_Data.Read_Result;
   begin
      Check (S.Table_Capacity = 40 and S.Byte_Capacity = Length + 39 * 36
             and S.Table_Byte_Limit = Length);
      Install (S, 1, DSDT, Named_Table (0, 0), Result);
      Check (Result = Installed);
      Install (S, 2, Description, Source, Result);
      Check (Result = Installed and Table_Info (S, 2).Extent = Length);
      Source := [others => 255];
      for I in 3 .. 40 loop
         Install (S, I, Description, Description_Table ("TINY", 36, I), Result);
         Check (Result = Installed);
      end loop;
      Check (Observe (S).Tables = 40 and Observe (S).Bytes = S.Byte_Capacity);
      Check (Find_Table (S, (Name => "LARG", others => <>)) = 2);
      Check (Table_Byte (S, 2, Length - 1) = Last_Byte);
      Declare_Table_Region (S, 0, "RGN0", (Name => "LARG", others => <>), Region, Bound);
      Check (Bound = Namespace.Bound);
      Declare_Table_Field (S, 0, "LAST", Region, (Length - 1) * 8, 8, Field, Bound);
      Check (Bound = Namespace.Bound);
      Bits := Read_Namespace_Field (S, Field);
      Check (Bits.Status = AML_Decode.Accepted and then Bits.Length = 1
             and then Bits.Content (1) = Last_Byte);
      Before := Snapshot (S);
      Install (S, 41, Description, Description_Table ("TINY", 36), Result);
      Check (Result = Table_Limit and Snapshot (S) = Before
             and Observe (S).Tables = 40 and Observe (S).Bytes = S.Byte_Capacity);
      Check (Table_Byte (S, 2, Length - 1) = Last_Byte);
   end;
   -- Exact fit and one-byte shortage are distinct from the per-table quota.
   for Case_ID in 0 .. 2 loop
      declare
         S : State := Fresh
           (3, (if Case_ID = 1 then 72 else 73),
            (if Case_ID = 2 then 36 else 37));
      begin
         Install (S, 1, DSDT, Named_Table (0, 0), Result);
         Check (Result = Installed);
         Before := Snapshot (S);
         Install (S, 2, Description, Description_Table ("TEST", 37), Result);
         Check (Result = (if Case_ID = 0 then Installed else Byte_Limit));
         Check (Snapshot (S) = Before);
         Check (Observe (S).Tables = (if Case_ID = 0 then 2 else 1));
         Check (Observe (S).Bytes = (if Case_ID = 0 then 73 else 36));
      end;
   end loop;
   FADT_Tests;
   Ada.Text_IO.Put_Line ("ACPI-SERVICE-CHECK: PASS" & Checks'Image);
end Service_Tests;
