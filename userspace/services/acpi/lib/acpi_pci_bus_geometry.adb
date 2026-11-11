pragma Ada_2022;
package body ACPI_PCI_Bus_Geometry with SPARK_Mode is
   subtype Refusal is Outcome range Invalid_Window .. Outcome'Last;
   function Plan_Config
     (Window : Bus_Window; Item : P.Region; Offset : P.Word;
      Width : P.T.Access_Width; For_Write : Boolean := False) return View
   is
      Bytes : constant P.Word := P.Word (P.T.Octets (Width));
      Config, First : P.Word;
   begin
      if not Valid (Window) then return (Status => Invalid_Window);
      elsif For_Write then return (Status => Write_Refused);
      elsif Item.Device.Segment /= Window.Segment then return (Status => Wrong_Segment);
      elsif Item.Device.Bus /= Window.Bus then return (Status => Wrong_Bus);
      elsif Width = P.T.Qword_Access then return (Status => Unsupported_Width);
      elsif not P.Valid_Region (Item) then return (Status => Invalid_Region);
      elsif Offset >= Item.Length or else Bytes > Item.Length - Offset then
         return (Status => Outside_Region);
      end if;
      Config := Item.Base + Offset;
      if Config mod Bytes /= 0 then return (Status => Misaligned); end if;
      First := (P.Word (Item.Device.Device) * P.Functions_Per_Device + P.Word (Item.Device.Func))
        * P.Function_Bytes + Config;
      return (Status => Read_Ready, First => P.ECAM_Offset (First),
        Bytes => P.Read_Bytes (Bytes), Width => Width);
   end Plan_Config;
   function Plan_Alias
     (Window : Bus_Window; Region_First, Region_Bytes, Offset : P.Word;
      Width : P.T.Access_Width; For_Write : Boolean := False) return Translation
   is
      Region_Start, Region_End, Datum_First, Ordinal : P.Word;
      Function_First, Clip_First, Clip_End : P.Word;
      Config : P.Region;
      Checked : View;
   begin
      if not Valid (Window) then return (Status => Invalid_Window);
      elsif For_Write then return (Status => Write_Refused);
      elsif Width = P.T.Qword_Access then return (Status => Unsupported_Width);
      elsif Region_Bytes = 0 then return (Status => Invalid_Region);
      elsif Region_First < Window.First then return (Status => Outside_Window);
      end if;
      Region_Start := Region_First - Window.First;
      if Region_Start >= Window.Bytes or else Region_Bytes > Window.Bytes - Region_Start then
         return (Status => Outside_Window);
      elsif Offset >= Region_Bytes or else P.Word (P.T.Octets (Width)) > Region_Bytes - Offset then
         return (Status => Outside_Region);
      end if;
      Region_End := Region_Start + Region_Bytes;
      Datum_First := Region_Start + Offset;
      Ordinal := Datum_First / P.Function_Bytes;
      Function_First := Ordinal * P.Function_Bytes;
      Clip_First := P.Word'Max (Region_Start, Function_First);
      Clip_End := P.Word'Min (Region_End, Function_First + P.Function_Bytes);
      Config :=
        (Device => (Segment => Window.Segment, Bus => Window.Bus,
          Device => P.Device_Number (Ordinal / P.Functions_Per_Device),
          Func => P.Function_Number (Ordinal mod P.Functions_Per_Device)),
         Base => Clip_First - Function_First, Length => Clip_End - Clip_First);
      Checked := Plan_Config (Window, Config, Datum_First - Clip_First, Width);
      if Checked.Status /= Read_Ready then
         declare Why : constant Refusal := Checked.Status; begin return (Status => Why); end;
      end if;
      return (Status => Read_Ready, Region => Config, Offset => Datum_First - Clip_First,
        First => Checked.First);
   end Plan_Alias;
end ACPI_PCI_Bus_Geometry;
