with Producer_Model;
package body ACPI_Native_Initializer_History is
   function Observe return Report is
   begin
      Producer_Model.History_Reads := Producer_Model.History_Reads + 1;
      if Producer_Model.Mode = 37 then raise Program_Error with "controlled history-metadata interruption"; end if;
      return (Complete => Producer_Model.History_Complete, others => <>);
   end Observe;
end ACPI_Native_Initializer_History;
