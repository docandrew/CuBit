pragma Ada_2022;
with System.Storage_Elements;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Windows;
package body ACPI_Backend_Memory_Access is
   use Interfaces;
   package T renames ACPI_FADT.Transactions;
   procedure Transfer
     (Virtual : Unsigned_64; Width : T.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      Limit : constant Unsigned_64 := ACPI_Backend_Binding_Types.User_Virtual_Limit;
   begin
      Output := 0; Completed := False;
      if Virtual = 0 or else Virtual >= Limit or else Unsigned_64 (T.Octets (Width)) > Limit - Virtual
        or else not T.Aligned (Virtual, Width)
        or else (if For_Write then Input > ACPI_Backend_Windows.Width_Mask (Width) else Input /= 0)
      then return; end if;
      declare
         Address : constant System.Address := System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Virtual));
      begin
         case Width is
            when T.Byte_Access =>
               declare Cell : Unsigned_8 with Import, Volatile_Full_Access, Address => Address;
               begin if For_Write then Cell := Unsigned_8 (Input); else Output := Unsigned_64 (Cell); end if; end;
            when T.Word_Access =>
               declare Cell : Unsigned_16 with Import, Volatile_Full_Access, Address => Address;
               begin if For_Write then Cell := Unsigned_16 (Input); else Output := Unsigned_64 (Cell); end if; end;
            when T.Dword_Access =>
               declare Cell : Unsigned_32 with Import, Volatile_Full_Access, Address => Address;
               begin if For_Write then Cell := Unsigned_32 (Input); else Output := Unsigned_64 (Cell); end if; end;
            when T.Qword_Access =>
               declare Cell : Unsigned_64 with Import, Volatile_Full_Access, Address => Address;
               begin if For_Write then Cell := Input; else Output := Cell; end if; end;
         end case;
      end;
      Completed := True;
   end Transfer;
end ACPI_Backend_Memory_Access;
