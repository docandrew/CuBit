pragma Ada_2022;
with Ada.Text_IO;
with AML_Field_Layout;
with AML_Register_Views;
with AML_Field_Plans;
with AML_Field_Datums;
with ACPI_PCI_Config_Reads;
procedure Geometry_Tests is
   package L renames AML_Field_Layout;
   package V renames AML_Register_Views;
   package F renames AML_Field_Plans;
   package P renames ACPI_PCI_Config_Reads;
   use type P.Word, P.Outcome, P.T.Access_Width, F.Datum_Count;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   function Region_For (Index : Natural) return L.Region_Geometry is
     ((Space => L.PCI_Config_Space, Base => 0, Length => 256,
       PCI => (Known => True, Device => (Segment => 0, Bus => 0,
         Device => P.Device_Number (Index / P.Functions_Per_Device),
         Func => P.Function_Number (Index mod P.Functions_Per_Device)))));
   Full : L.Field_Geometry := (Bit_Offset => 16#60# * L.Bits_Per_Byte,
     Bit_Length => 32, Flags => 3, Effective_Access => 3, others => <>);
   Partial : L.Field_Geometry;
   First, Other : L.Register_Geometry;
   Plan : F.Plan;
   One_Bus_Functions : constant := P.Devices_Per_Bus * P.Functions_Per_Device;
begin
   for A in 0 .. One_Bus_Functions - 1 loop
      Full.Region := Region_For (A);
      First := (Region => Full.Region, Byte_Offset => 16#60#, Width => P.T.Dword_Access);
      Partial := (Full with delta Bit_Offset => 16#60# * L.Bits_Per_Byte + 26, Bit_Length => 6);
      Plan := F.Prepare (Partial);
      Check (V.Valid_Register (First) and then F.Valid (Plan) and then F.Count (Plan) = 1);
      declare D : constant AML_Field_Datums.Datum := F.Datum_At (Plan, 1); begin
         Check (P.Word (D.Offset) = 16#60# and then D.Access_Width = P.T.Dword_Access
           and then D.Mask = 16#FC00_0000#);
      end;
      for B in 0 .. One_Bus_Functions - 1 loop
         Other := (Region => Region_For (B), Byte_Offset => 16#60#, Width => P.T.Dword_Access);
         Check (V.Same_Transaction (First, Other) = (A = B));
         Check (V.Can_Rebase (First, Other.Region) = (A = B));
         Check (V.Rebase (First, Other.Region).Allowed = (A = B));
         Check (F.Matches (Plan, 1, Other) = (A = B));
         Check (L.Admitted (Full, Other) = (A = B));
         Check (V.Admitted (Full, Other) = (A = B));
      end loop;
   end loop;
   Full.Region := Region_For (0);
   First := (Full.Region, 16#60#, P.T.Dword_Access);
   declare
      Alias_Region : L.Region_Geometry := (Full.Region with delta Base => 16#60#, Length => 4);
      Rebased : constant V.View := V.Rebase (First, Alias_Region);
   begin
      Check (Rebased.Allowed and then Rebased.Value.Byte_Offset = 0
        and then V.Same_Transaction (First, Rebased.Value));
      Alias_Region.PCI.Device.Segment := 1;
      Check (not V.Rebase (First, Alias_Region).Allowed);
      Alias_Region.PCI.Device.Segment := 0; Alias_Region.PCI.Device.Bus := 1;
      Check (not V.Rebase (First, Alias_Region).Allowed);
      Alias_Region.PCI := (Known => False);
      Check (not V.Valid_Region (Alias_Region) and then not V.Rebase (First, Alias_Region).Allowed);
   end;
   Full.Region.PCI := (Known => False); First.Region := Full.Region;
   Check (not L.Domain_Valid (Full.Region) and then not V.Valid_Register (First)
     and then not F.Valid (F.Prepare (Full)) and then not L.Admitted (Full, First));
   Full.Region := Region_For (0); Full.Region.Space := L.Memory_Space; First.Region := Full.Region;
   Check (not L.Domain_Valid (Full.Region) and then not V.Valid_Register (First)
     and then not F.Valid (F.Prepare (Full)) and then not L.Admitted (Full, First));
   Full.Region.PCI := (Known => False); First.Region := Full.Region;
   Check (V.Valid_Register (First) and then L.Admitted (Full, First));
   Full.Region := Region_For (0); Full.Flags := 16#13#;
   Check (not F.Valid (F.Prepare (Full, Allow_Locked_Read => True)));
   Full.Flags := 4; Full.Effective_Access := 4; Full.Bit_Offset := 0; Full.Bit_Length := 64;
   Check (not F.Valid (F.Prepare (Full)));
   First := (Full.Region, 0, P.T.Qword_Access);
   Check (not V.Valid_Register (First) and then not L.Admitted (Full, First));
   Full := (Region => Region_For (255), Bit_Offset => 0, Bit_Length => 8,
     Flags => 1, Effective_Access => 1, others => <>);
   Full.Region.Base := P.Function_Bytes - 1; Full.Region.Length := 1;
   First := (Full.Region, 0, P.T.Byte_Access);
   Check (V.Valid_Register (First) and then L.Admitted (Full, First));
   Full.Region.Length := 2; First.Region := Full.Region;
   Check (not V.Valid_Register (First) and then not F.Valid (F.Prepare (Full)));
   Full.Region := Region_For (0); Full.Region.PCI.Device.Bus := 1;
   Check (V.Valid_Region (Full.Region) and then
     P.Plan ((Full.Region.PCI.Device, Full.Region.Base, Full.Region.Length), 0,
       P.T.Byte_Access, P.Bus_Bytes).Status = P.Unsupported_Bus);
   Ada.Text_IO.Put_Line ("PCI Field geometry checks" & Checks'Image);
end Geometry_Tests;
