with Ada.Text_IO;
with AML_Decode;
with ACPI_Fatal_Diagnostics; use ACPI_Fatal_Diagnostics;
procedure Fatal_Format_Tests is
   use type AML_Decode.Integer_Width;
   Existing_Log_Record_Bound : constant Positive := 512;
   pragma Compile_Time_Error
     (Maximum_Text_Length > Existing_Log_Record_Bound,
      "Fatal diagnostic exceeds existing logger record bound");
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
begin
   Check (Format ((Present => False)) = "acpi fatal: none", "absence format");
   for Width in AML_Decode.Integer_Width loop
      for Kind in Origin loop
         declare
            Seen : constant Observation := (Present => True,
              Item => (Literal => (Kind => 16#FE#, Code => 16#FEDCBA98#),
                Evaluated_Argument => 16#FEDCBA98_76543210#, Width => Width),
              Source => (if Kind = Definition_Block then (Kind => Definition_Block, Table_ID => 41)
                else (Kind => Device_Initializer, Initializer_Scope => 0)));
            Text : constant String := Format (Seen);
            Expected : constant String := "acpi fatal origin=" &
              (if Kind = Definition_Block then "module table=0000000000000029"
               else "initializer scope=0000000000000000") &
              " type=FE code=FEDCBA98 arg=FEDCBA9876543210 width=" &
              (if Width = AML_Decode.Bits_32 then "32" else "64");
         begin
            Check (Text = Expected, "formatter preserves raw64 and width separately");
            Check (Text'Length <= Maximum_Text_Length,
              "fits existing logger record bound");
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Fatal format checks" & Checks'Image);
end Fatal_Format_Tests;
