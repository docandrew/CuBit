package body CCL.Manifests.Model with SPARK_Mode => On is
   function Valid_Binding_Name (Item : String) return Boolean is
   begin
      if Item'Length not in 1 .. Binding_Name_Length'Last or else Item (Item'First) not in 'a' .. 'z' then
         return False;
      end if;
      for Index in Item'Range loop
         if Item (Index) = '-' then
            if Index = Item'Last or else Item (Index + 1) = '-' then return False; end if;
         elsif Item (Index) not in 'a' .. 'z' | '0' .. '9' then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Binding_Name;

   function Valid_Scope_Path (Path : String) return Boolean is
   begin
      if Path'Length not in 1 .. Binding_Name_Length'Last then return False; end if;
      for I in Path'Range loop
         declare
            C : constant Character := Path (I);
         begin
            if C not in ' ' .. '~' or else C in '*' | '?' | '\' then return False; end if;
            if C = '.' and then (I = Path'First or else Path (I - 1) = '/') then
               if I = Path'Last or else Path (I + 1) = '/' then
                  return False;
               elsif Path (I + 1) = '.' and then (I + 1 = Path'Last or else Path (I + 2) = '/') then
                  return False;
               end if;
            end if;
            if C = '/' and then I < Path'Last and then Path (I + 1) = '/' then return False; end if;
         end;
      end loop;
      return True;
   end Valid_Scope_Path;
   procedure Add_Drive
     (Decl : in out Declaration; Item : Devices.Device_Match;
      Accepted : out Boolean; Diagnostic : out Diagnostic_Code)
   is
      use type Devices.Match_Kind;
      use type Devices.Device_Match;
      use type Sections.Basis_Info;
      Wanted : Sections.Basis_Info;
   begin
      Accepted := False;
      Diagnostic := Invalid_Device_Match;
      if not Devices.Valid_Match (Item) then
         return;
      end if;
      if Decl.Drives.Count = Devices.MAX_ENTRIES then
         Diagnostic := Too_Many_Requests;
         return;
      end if;
      for I in 1 .. Decl.Drives.Count loop
         if Decl.Drives.Entries (I) = Item then
            Diagnostic := Duplicate_Device_Match;
            return;
         end if;
      end loop;
      Wanted := Decl.Basis;
      case Item.Kind is
         when Devices.PCI_IDs | Devices.PCI_Class =>
            Wanted := (Kind => PCI_Basis, Values => [others => 0]);
         when Devices.Platform =>
            Wanted := (Kind => Platform_Basis,
                       Values => [1 => Unsigned_16 (Platform_Device'Enum_Rep (Item.Device)),
                                  others => 0]);
         when Devices.ACPI_ID | Devices.USB_Class =>
            null;
      end case;
      if Decl.Basis.Kind /= No_Basis and then Wanted /= Decl.Basis then
         --  Resource indexes refer to one kind of device only.
         return;
      end if;
      Decl.Basis := Wanted;
      Decl.Drives.Count := Decl.Drives.Count + 1;
      Decl.Drives.Entries (Decl.Drives.Count) := Item;
      Accepted := True;
      Diagnostic := No_Error;
   end Add_Drive;
end CCL.Manifests.Model;
