pragma Ada_2022;
package body Owner_Test_Control is
   procedure Write (Severity : Level; Text : String) is
   begin
      if Log_Count = Log_Capacity or else Text'Length > Text_Capacity then
         raise Program_Error with "owner diagnostic overflow";
      end if;
      Log_Count := Log_Count + 1;
      Log (Log_Count) := (Severity, Text'Length, [others => ' ']);
      Log (Log_Count).Text (1 .. Text'Length) := Text;
      if Severity = Critical then
         Critical_Attempts := Critical_Attempts + 1;
         if Broken_Critical then raise Program_Error with "modeled critical logger failure"; end if;
      end if;
   end Write;
end Owner_Test_Control;
