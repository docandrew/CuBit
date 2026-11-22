with Producer_Model; with Sender_Model;
package body ACPI_Native_Pre_Device is
   function Submitted return Submitted_Request is
   begin
      Producer_Model.Submitted_Reads := Producer_Model.Submitted_Reads + 1;
      if Producer_Model.Mode = 36 then raise Program_Error with "controlled submitted-metadata interruption"; end if;
      if Producer_Model.Mode = 4 then return (Present => False); end if;
      return (Present => True, Data => (Epoch => (if Producer_Model.Mode = 7 then 1 else Sender_Model.Own_ID)),
        Device => 90, Request_ID => 1, Receipt_Known => Producer_Model.Mode /= 5,
        Answer => (if Producer_Model.Mode = 6 then CuBit.GPIO_Bootstrap_Exchange.Malformed
                   elsif Producer_Model.Mode = 29 then CuBit.GPIO_Bootstrap_Exchange.P2SB_Available
                   else CuBit.GPIO_Bootstrap_Exchange.Admitted));
   end Submitted;
   function Quiet return Boolean is (Producer_Model.Mode /= 30);
end ACPI_Native_Pre_Device;
