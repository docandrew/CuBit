pragma Ada_2022;
with ACPI_Backend_Memory_Access;
with ACPI_Backend_Port_Access;
package body ACPI_Backend_Global_Signal_Native is
   use Interfaces;
   package C renames ACPI_PM1_Global_Release;
   use type C.Completion;
   procedure Transfer_Word
     (View : Bindings.Access_View; For_Write : Boolean; Input : Unsigned_16;
      Output : out Unsigned_16; Status : out C.Completion)
   is
      Wide : Unsigned_64;
      Completed : Boolean;
   begin
      Output := 0; Status := C.Not_Attempted;
      if not View.Allowed then return; end if;
      case View.Space is
         when Bindings.P.Memory_Space =>
            ACPI_Backend_Memory_Access.Transfer
              (View.Virtual_Address, Bindings.T.Word_Access, For_Write, Unsigned_64 (Input), Wide, Completed);
         when Bindings.P.IO_Space =>
            ACPI_Backend_Port_Access.Transfer
              (View.Ports, View.Port, Bindings.T.Word_Access, For_Write, Unsigned_64 (Input), Wide, Completed);
      end case;
      Status := C.Classify_Transfer (For_Write, Completed, Wide);
      if Status = C.Completed and then not For_Write then
         Output := Unsigned_16 (Wide);
      end if;
   end Transfer_Word;
end ACPI_Backend_Global_Signal_Native;
