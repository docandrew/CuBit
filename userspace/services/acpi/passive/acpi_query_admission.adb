pragma Ada_2022;
package body ACPI_Query_Admission with SPARK_Mode is
   function Admit
     (Sender : PID.Process_ID; Stamp : ACPI_Protocol.Role_Tag; Decoded : Boolean;
      Reference_Word, Declared_Length, Request_Capacity, Response_Capacity : Wire.Word)
      return Decision is
   begin
      if not ACPI_Protocol.Is_Observer (Stamp) then return (Status => Denied); end if;
      if not Decoded then return (Status => Malformed); end if;
      if not PID.Is_Process (Sender) then return (Status => Invalid_Sender); end if;
      if not References.Valid_Wire (Reference_Word) then return (Status => Invalid_Reference); end if;
      if Declared_Length < Pages.Frame_Byte_Count
        or else Declared_Length > Wire.Word (Natural'Last)
        or else Declared_Length > Request_Capacity or else Declared_Length > Response_Capacity
      then return (Status => Invalid_Extent); end if;
      return (Status => Admitted,
        Value => (Sender, References.Decode (Reference_Word), Declared_Length));
   end Admit;
end ACPI_Query_Admission;
