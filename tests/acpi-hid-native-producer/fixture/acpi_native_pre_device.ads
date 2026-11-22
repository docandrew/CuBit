with Interfaces; with GPIO_Bootstrap_Payload; with ACPI_Service;
with CuBit.GPIO_Bootstrap_Exchange;
package ACPI_Native_Pre_Device is
   type Submitted_Request (Present : Boolean := False) is record
      case Present is
         when True =>
            Data : GPIO_Bootstrap_Payload.Request;
            Device : ACPI_Service.Node_Descriptor;
            Request_ID : Interfaces.Unsigned_64;
            Receipt_Known : Boolean;
            Answer : CuBit.GPIO_Bootstrap_Exchange.Disposition;
         when False => null;
      end case;
   end record;
   function Submitted return Submitted_Request;
   function Quiet return Boolean;
end ACPI_Native_Pre_Device;
