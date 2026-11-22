pragma Ada_2022;
with CuBit.Messages;
with Interfaces;
package body ACPI_Backend_Loop_Native is
   use type Interfaces.Unsigned_64;
   procedure Poll_Completion (Found : out Boolean) is
      Completion : CuBit.Messages.CompletionEntry;
   begin Found := CuBit.Messages.Poll_Completion (Completion'Address) /= 0; end Poll_Completion;
end ACPI_Backend_Loop_Native;
