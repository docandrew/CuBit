pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_PCI_Config_Reads;
procedure Policy_Tests is
   package P renames ACPI_PCI_Config_Reads;
   use type P.Outcome, P.T.Access_Width;
   use type P.Word;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Expect (R : P.Region; Offset : P.Word; W : P.T.Access_Width;
      Expected : P.Outcome; Window : P.Word := P.Bus_Bytes; Writing : Boolean := False) is
      V : constant P.View := P.Plan (R, Offset, W, Window, Writing);
   begin Check (V.Status = Expected); end Expect;
   R : P.Region := (Device => (others => <>), Base => 0, Length => P.Function_Bytes);
   Dev_Shift : constant := 15;
   Func_Shift : constant := 12;
begin
   -- Exhaust all byte offsets, three read widths and all bus-zero functions.
   for D in P.Device_Number loop
      R.Device.Device := D;
      for F in P.Function_Number loop
         R.Device.Func := F;
         for O in P.Word range 0 .. P.Function_Bytes - 1 loop
            for W in P.T.Byte_Access .. P.T.Dword_Access loop
               declare
                  V : constant P.View := P.Plan (R, O, W, P.Bus_Bytes);
                  Bytes : constant P.Word := P.Word (P.T.Octets (W));
                  Expected : constant P.Word :=
                    Interfaces.Shift_Left (P.Word (D), Dev_Shift)
                    or Interfaces.Shift_Left (P.Word (F), Func_Shift) or O;
               begin
                  if Bytes > P.Function_Bytes - O then Check (V.Status = P.Outside_Region);
                  elsif O mod Bytes /= 0 then Check (V.Status = P.Misaligned);
                  else
                     Check (V.Status = P.Read_Ready and then P.Word (V.First) = Expected
                       and then V.Bytes = P.T.Octets (W) and then V.Width = W
                       and then P.Word (V.First) + Bytes <= P.Bus_Bytes);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   R.Device := (others => <>);
   Expect (R, 0, P.T.Byte_Access, P.Invalid_Window, 0);
   Expect (R, 0, P.T.Byte_Access, P.Invalid_Window, P.Bus_Bytes - 1);
   Expect (R, 0, P.T.Byte_Access, P.Invalid_Window, P.Bus_Bytes + 1);
   Expect (R, 0, P.T.Byte_Access, P.Invalid_Window, P.Word'Last);
   for W in P.T.Access_Width loop Expect (R, 0, W, P.Write_Refused, Writing => True); end loop;
   R.Device.Segment := P.Segment_Number'Last;
   Expect (R, 0, P.T.Byte_Access, P.Unsupported_Segment);
   R.Device.Segment := 0; R.Device.Bus := P.Bus_Number'Last;
   Expect (R, 0, P.T.Byte_Access, P.Unsupported_Bus);
   R.Device.Bus := 0;
   Expect (R, 0, P.T.Qword_Access, P.Unsupported_Width);
   Expect (R, P.Function_Bytes, P.T.Byte_Access, P.Outside_Region);
   Expect (R, P.Word'Last, P.T.Dword_Access, P.Outside_Region);
   R.Length := 0; Expect (R, 0, P.T.Byte_Access, P.Invalid_Region);
   R.Length := P.Word'Last; Expect (R, 0, P.T.Byte_Access, P.Invalid_Region);
   R.Length := 1; R.Base := P.Word'Last; Expect (R, 0, P.T.Byte_Access, P.Invalid_Region);
   R.Base := P.Function_Bytes; Expect (R, 0, P.T.Byte_Access, P.Invalid_Region);
   R.Base := P.Function_Bytes - 1; R.Length := 2; Expect (R, 0, P.T.Byte_Access, P.Invalid_Region);
   R.Length := 1; Expect (R, 0, P.T.Byte_Access, P.Read_Ready);
   Expect (R, 0, P.T.Word_Access, P.Outside_Region);
   R.Base := 3; R.Length := 5;
   Expect (R, 0, P.T.Word_Access, P.Misaligned);
   Expect (R, 1, P.T.Dword_Access, P.Read_Ready);
   Expect (R, 2, P.T.Dword_Access, P.Outside_Region);
   Ada.Text_IO.Put_Line ("PCI config policy checks" & Checks'Image);
end Policy_Tests;
