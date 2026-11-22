pragma Ada_2022;
package body ACPI_PCI_Memory_Reads with SPARK_Mode is
   use type P.T.Access_Width;
   subtype Refusal is P.Outcome range P.Invalid_Window .. P.Outcome'Last;
   function Plan
     (Window_First, Window_Bytes, Region_First, Region_Bytes, Offset : P.Word;
      Width : P.T.Access_Width; For_Write : Boolean := False) return Translation
   is
      Region_Start, Region_End, Datum_First, Function_First, Function_End : P.Word;
      Clip_First, Clip_End, Function_Ordinal : P.Word;
      Config : P.Region;
      Checked : P.View;
   begin
      if Window_Bytes /= P.Bus_Bytes
        or else Window_First mod P.Function_Bytes /= 0
        or else Window_Bytes - 1 > P.Word'Last - Window_First
      then return (Status => P.Invalid_Window);
      elsif For_Write then return (Status => P.Write_Refused);
      elsif Width = P.T.Qword_Access then return (Status => P.Unsupported_Width);
      elsif Region_Bytes = 0 or else Region_First < Window_First then
         return (Status => P.Invalid_Region);
      end if;
      Region_Start := Region_First - Window_First;
      if Region_Start >= Window_Bytes or else Region_Bytes > Window_Bytes - Region_Start then
         return (Status => P.Invalid_Region);
      elsif Offset >= Region_Bytes or else P.Word (P.T.Octets (Width)) > Region_Bytes - Offset then
         return (Status => P.Outside_Region);
      end if;
      Region_End := Region_Start + Region_Bytes;
      Datum_First := Region_Start + Offset;
      Function_Ordinal := Datum_First / P.Function_Bytes;
      Function_First := Function_Ordinal * P.Function_Bytes;
      Function_End := Function_First + P.Function_Bytes;
      -- Retain the intersection with this function, including declarations
      -- that span several functions. No intersection extends the AML region.
      Clip_First := P.Word'Max (Region_Start, Function_First);
      Clip_End := P.Word'Min (Region_End, Function_End);
      Config :=
        (Device => (Segment => 0, Bus => 0,
          Device => P.Device_Number (Function_Ordinal / P.Functions_Per_Device),
          Func => P.Function_Number (Function_Ordinal mod P.Functions_Per_Device)),
         Base => Clip_First - Function_First, Length => Clip_End - Clip_First);
      Checked := P.Plan (Config, Datum_First - Clip_First, Width, Window_Bytes);
      if Checked.Status /= P.Read_Ready then
         declare Why : constant Refusal := Checked.Status;
         begin return (Status => Why); end;
      end if;
      return (Status => P.Read_Ready, Region => Config,
        Offset => Datum_First - Clip_First, First => Checked.First);
   end Plan;
end ACPI_PCI_Memory_Reads;
