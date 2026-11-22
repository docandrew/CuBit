pragma Ada_2022;
package body ACPI_Native_Server.With_Power_Entry is
   use type Interfaces.Unsigned_64;
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
      Worked : Boolean;
   begin
      Advance_Entry (Server, Worked);
      if Blocks_Runtime then Result := (if Worked or else Entry_Runnable then Owner_Pending else Owner_Idle);
      else Advance_Normal (Server, Result); end if;
   end Advance;
   procedure Maintain (Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64; May_Continue : out Boolean) is
      Entry_Wake : constant Interfaces.Unsigned_64 := Entry_Deadline_Milliseconds;
   begin
      Maintain_Normal (Wake_Deadline_Milliseconds, May_Continue);
      if Entry_Wake /= CuBit.Messages.Wait_Forever then
         Wake_Deadline_Milliseconds := Interfaces.Unsigned_64'Min (Wake_Deadline_Milliseconds, Entry_Wake);
      end if;
   end Maintain;
   function Diagnostics_Quiet (Server : ACPI_Requests.State) return Boolean is
     (not Blocks_Runtime and then Diagnostics_Normal (Server));
end ACPI_Native_Server.With_Power_Entry;
