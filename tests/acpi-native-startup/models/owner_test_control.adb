pragma Ada_2022;
package body Owner_Test_Control is
   procedure Write (Severity : Level; Text : String) is
      Recovery_Prefix : constant String := "acpi: stage aml-recovery ok ";
   begin
      if Log_Count = Log_Capacity or else Text'Length > Text_Capacity then
         raise Program_Error with "owner diagnostic overflow";
      end if;
      Log_Count := Log_Count + 1;
      Log (Log_Count) := (Severity, Text'Length, [others => ' ']);
      Log (Log_Count).Text (1 .. Text'Length) := Text;
      if Severity = Warning and then Text'Length >= Recovery_Prefix'Length
        and then Text (Text'First .. Text'First + Recovery_Prefix'Length - 1) = Recovery_Prefix
      then
         Recovery_Attempts := Recovery_Attempts + 1;
         if Selected = Recovery_Log_Failure then raise Program_Error with "modeled recovery logger failure"; end if;
      end if;
      if Severity = Critical then
         Critical_Attempts := Critical_Attempts + 1;
         if Broken_Critical then raise Program_Error with "modeled critical logger failure"; end if;
      end if;
   end Write;
end Owner_Test_Control;
