pragma Ada_2022;
with CuBit.Handles.Devices;
with ACPI_Backend_Windows;
package body ACPI_Backend_Port_Access is
   use Interfaces;
   package T renames ACPI_FADT.Transactions;
   use type T.Access_Width;
   procedure Transfer
     (Authority : CuBit.Handles.IO_Ports; Port : Unsigned_16;
      Width : T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      Value : Unsigned_64;
   begin
      Output := 0; Completed := False;
      if Width = T.Qword_Access or else
        (if For_Write then Input > ACPI_Backend_Windows.Width_Mask (Width) else Input /= 0)
      then return; end if;
      if For_Write then
         case Width is
            when T.Byte_Access => Completed := CuBit.Handles.Devices.Out_8 (Authority, Port, Unsigned_8 (Input));
            when T.Word_Access => Completed := CuBit.Handles.Devices.Out_16 (Authority, Port, Unsigned_16 (Input));
            when T.Dword_Access => Completed := CuBit.Handles.Devices.Out_32 (Authority, Port, Unsigned_32 (Input));
            when T.Qword_Access => null;
         end case;
      else
         case Width is
            when T.Byte_Access => Value := CuBit.Handles.Devices.In_8 (Authority, Port);
            when T.Word_Access => Value := CuBit.Handles.Devices.In_16 (Authority, Port);
            when T.Dword_Access => Value := CuBit.Handles.Devices.In_32 (Authority, Port);
            when T.Qword_Access => return;
         end case;
         if Value > ACPI_Backend_Windows.Width_Mask (Width) then return; end if;
         Output := Value; Completed := True;
      end if;
   end Transfer;
end ACPI_Backend_Port_Access;
