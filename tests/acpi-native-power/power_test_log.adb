with Ada.Strings.Fixed;
with Ada.Command_Line;
package body Power_Test_Log is
   Thrown : Boolean := False;
   procedure Emit (Text : String) is
      function Starts (Prefix : String) return Boolean is
        (Ada.Strings.Fixed.Index (Text, Prefix) = 1);
   begin
      if Text'Length not in 1 .. 512 then raise Program_Error with "bounded stage line"; end if;
      if not Thrown and then Ada.Command_Line.Argument (2) = "log-error"
        and then Starts ("acpi: stage lid-observation ok") then
         Thrown := True; raise Program_Error with "modeled logging exception";
      end if;
      Total := Total + 1;
      if Starts ("acpi: stage sleep-registers ") then Sleep_Registers := Sleep_Registers + 1;
      elsif Starts ("acpi: stage lid-observation ok") then Lids := Lids + 1;
      elsif Starts ("acpi: stage ac-observation ok") then Sources := Sources + 1;
      elsif Starts ("acpi: stage battery-observation ok OBSERVED") then
         Batteries := Batteries + 1;
         if Ada.Strings.Fixed.Index (Text, "units=CHARGE_AND_CURRENT") = 0 or else
           (Ada.Strings.Fixed.Index (Text, "remaining= 20000") = 0 and then
            Ada.Strings.Fixed.Index (Text, "remaining= 5000") = 0) then
            raise Program_Error with "actual BIX units and BST quantity";
         end if;
      elsif Starts ("acpi: stage battery-observation ok ABSENT") then Absent := Absent + 1;
      elsif Starts ("acpi: stage power-discovery ") then Summaries := Summaries + 1;
      elsif not Starts ("acpi: stage ") then raise Program_Error with "unexpected log";
      end if;
   end Emit;
end Power_Test_Log;
