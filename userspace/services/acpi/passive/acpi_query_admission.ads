pragma Ada_2022;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with ACPI_Protocol;
with ACPI_Passive_Query;
with ACPI_Passive_Query.Pages;
package ACPI_Query_Admission with SPARK_Mode, Pure is
   package PID renames CuBit.Process_IDs;
   package References renames CuBit.Grant_References;
   package Wire renames ACPI_Passive_Query;
   package Pages renames ACPI_Passive_Query.Pages;
   type Outcome is (Admitted, Denied, Malformed, Invalid_Sender,
                    Invalid_Reference, Invalid_Extent);
   type Query is record
      Sender : PID.Process_ID := PID.No_Process;
      Loan : References.Reference;
      Length : Wire.Word := 0;
   end record;
   type Decision (Status : Outcome := Malformed) is record
      case Status is
         when Admitted => Value : Query;
         when others => null;
      end case;
   end record;
   use type PID.Process_ID;
   use type Wire.Word;
   -- Sender/stamp come exclusively from the kernel-received message. Decoded
   -- means the generated typed decoder accepted all framing/reserved fields.
   -- Capacities are trusted lengths of the owner's private copy buffers.
   -- References remain identities; admission does not acquire a mapping or
   -- establish the grant's owner, liveness or access rights. Acquire must still
   -- use Value.Sender as expected owner and Value.Length as the exact extent.
   function Admit
     (Sender : PID.Process_ID; Stamp : ACPI_Protocol.Role_Tag; Decoded : Boolean;
      Reference_Word, Declared_Length, Request_Capacity, Response_Capacity : Wire.Word)
      return Decision
     with Global => null,
       Post => (if not ACPI_Protocol.Is_Observer (Stamp) then Admit'Result.Status = Denied)
         and then (if Admit'Result.Status = Admitted then
           Decoded and then ACPI_Protocol.Is_Observer (Stamp)
           and then Admit'Result.Value.Sender = Sender and then PID.Is_Process (Sender)
           and then References.Valid_Wire (Reference_Word)
           and then References.Encode (Admit'Result.Value.Loan) = Reference_Word
           and then Admit'Result.Value.Length = Declared_Length
           and then Declared_Length >= Pages.Frame_Byte_Count
           and then Declared_Length <= Wire.Word (Natural'Last)
           and then Declared_Length <= Request_Capacity
           and then Declared_Length <= Response_Capacity);
end ACPI_Query_Admission;
