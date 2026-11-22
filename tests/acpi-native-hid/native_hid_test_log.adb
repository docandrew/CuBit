with Ada.Strings.Fixed;
with Ada.Command_Line;
package body Native_HID_Test_Log is
   Thrown : Boolean := False;
   procedure Emit (Text : String; Warning : Boolean) is
   begin
      if Text'Length not in 1 .. 512 then raise Program_Error with "bounded log line"; end if;
      if not Thrown and then Ada.Command_Line.Argument (2) = "log-error" and then
        Ada.Strings.Fixed.Index (Text, "acpi: stage hid-resource ok") = 1 then
         Thrown := True; raise Program_Error with "modeled logging exception";
      end if;
      Total := Total + 1;
      if Warning then Warnings := Warnings + 1; end if;
      if Ada.Strings.Fixed.Index (Text, "acpi: stage hid-resource ok OBSERVED") = 1 then
         Facts := Facts + 1;
         if Ada.Strings.Fixed.Index (Text, " address= 21 descriptor= 32 pin= 42") = 0 then
            raise Program_Error with "actual firmware resource facts";
         end if;
      elsif Ada.Strings.Fixed.Index (Text, "acpi: stage hid-discovery ") = 1 then
         Summaries := Summaries + 1;
      else raise Program_Error with "unexpected discovery log";
      end if;
   end Emit;
end Native_HID_Test_Log;
