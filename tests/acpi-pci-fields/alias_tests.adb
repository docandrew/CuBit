pragma Ada_2022;
with Ada.Text_IO;
with ACPI_PCI_Memory_Reads;
procedure Alias_Tests is
   package M renames ACPI_PCI_Memory_Reads;
   package P renames M.P;
   use type P.Word, P.Outcome, P.ECAM_Offset, P.T.Access_Width;
   Checks : Natural := 0;
   Root : constant P.Word := 16#E000_0000#;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Read_Is (Base, Bytes, Offset : P.Word; Width : P.T.Access_Width) is
      Value : constant M.Translation := M.Plan (Root, P.Bus_Bytes, Base, Bytes, Offset, Width);
   begin
      Check (Value.Status = P.Read_Ready);
      Check (P.Word (Value.First) = Base - Root + Offset);
      declare Config : constant P.View := P.Plan (Value.Region, Value.Offset, Width, P.Bus_Bytes); begin
         Check (Config.Status = P.Read_Ready and then Config.First = Value.First);
      end;
      Check (Value.Region.Length > 0 and then Value.Region.Length <= P.Function_Bytes - Value.Region.Base);
      Check (Value.Region.Base + Value.Offset = P.Word (Value.First) mod P.Function_Bytes);
   end Read_Is;
   procedure Refuse (Base, Bytes, Offset : P.Word; Width : P.T.Access_Width; Why : P.Outcome) is
      Value : constant M.Translation := M.Plan (Root, P.Bus_Bytes, Base, Bytes, Offset, Width);
   begin Check (Value.Status = Why); end Refuse;
begin
   for Function_Ordinal in P.Word range 0 .. P.Devices_Per_Bus * P.Functions_Per_Device - 1 loop
      for Width in P.T.Byte_Access .. P.T.Dword_Access loop
         declare Size : constant P.Word := P.Word (P.T.Octets (Width));
         begin
            Read_Is (Root + Function_Ordinal * P.Function_Bytes, P.Function_Bytes, 0, Width);
            Read_Is (Root + Function_Ordinal * P.Function_Bytes, P.Function_Bytes, P.Function_Bytes - Size, Width);
            Read_Is (Root, P.Bus_Bytes, Function_Ordinal * P.Function_Bytes, Width);
            Read_Is (Root + Function_Ordinal * P.Function_Bytes + 16#100#, 16#380#, 16#100#, Width);
         end;
      end loop;
   end loop;
   -- Full-window and crossing-function declarations keep their exact extent.
   for Offset in P.Word range 0 .. P.Bus_Bytes - 1 loop
      declare Value : constant M.Translation := M.Plan (Root, P.Bus_Bytes, Root, P.Bus_Bytes, Offset, P.T.Byte_Access); begin
         Check (Value.Status = P.Read_Ready and then P.Word (Value.First) = Offset);
      end;
   end loop;
   Read_Is (Root + P.Function_Bytes - 4, 8, 4, P.T.Dword_Access);
   Read_Is (Root, 16#30#, 0, P.T.Dword_Access);
   Read_Is (Root + 16#D8000#, 16#480#, 16#47C#, P.T.Dword_Access);
   Refuse (Root - 1, 1, 0, P.T.Byte_Access, P.Invalid_Region);
   Refuse (Root + P.Bus_Bytes - 1, 2, 0, P.T.Byte_Access, P.Invalid_Region);
   Refuse (Root, 0, 0, P.T.Byte_Access, P.Invalid_Region);
   Refuse (Root, P.Word'Last, 0, P.T.Byte_Access, P.Invalid_Region);
   Refuse (Root, 4, 4, P.T.Byte_Access, P.Outside_Region);
   Refuse (Root, 4, 3, P.T.Word_Access, P.Outside_Region);
   Refuse (Root, 8, 1, P.T.Dword_Access, P.Misaligned);
   Refuse (Root + P.Function_Bytes - 1, 2, 0, P.T.Word_Access, P.Outside_Region);
   Refuse (Root, 8, 0, P.T.Qword_Access, P.Unsupported_Width);
   Check (M.Plan (Root, P.Bus_Bytes, Root, 4, 0, P.T.Dword_Access, True).Status = P.Write_Refused);
   Check (M.Plan (Root, P.Bus_Bytes - 1, Root, 4, 0, P.T.Dword_Access).Status = P.Invalid_Window);
   Check (M.Plan (Root + 1, P.Bus_Bytes, Root, 4, 0, P.T.Dword_Access).Status = P.Invalid_Window);
   Check (M.Plan (P.Word'Last - P.Function_Bytes + 1, P.Bus_Bytes, Root, 4, 0, P.T.Dword_Access).Status = P.Invalid_Window);
   Ada.Text_IO.Put_Line ("PCI memory alias checks" & Checks'Image & " PASS");
end Alias_Tests;
