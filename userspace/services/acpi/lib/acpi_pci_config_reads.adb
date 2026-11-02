pragma Ada_2022;
package body ACPI_PCI_Config_Reads with SPARK_Mode is
   function Plan
     (Item : Region; Offset : Word; Width : T.Access_Width;
      Window_Bytes : Word; For_Write : Boolean := False) return View
   is
      Bytes : constant Word := Word (T.Octets (Width));
      Config : Word;
      First : Word;
   begin
      if Window_Bytes /= Bus_Bytes then return (Status => Invalid_Window);
      elsif For_Write then return (Status => Write_Refused);
      elsif Item.Device.Segment /= 0 then return (Status => Unsupported_Segment);
      elsif Item.Device.Bus /= 0 then return (Status => Unsupported_Bus);
      elsif Width = T.Qword_Access then return (Status => Unsupported_Width);
      elsif not Valid_Region (Item) then return (Status => Invalid_Region);
      elsif Offset >= Item.Length or else Bytes > Item.Length - Offset then
         return (Status => Outside_Region);
      end if;
      -- Both additions stay within one function after subtraction-first checks.
      Config := Item.Base + Offset;
      if Config mod Bytes /= 0 then return (Status => Misaligned); end if;
      First := (Word (Item.Device.Device) * Functions_Per_Device + Word (Item.Device.Func))
        * Function_Bytes + Config;
      return (Status => Read_Ready, First => ECAM_Offset (First),
        Bytes => Read_Bytes (Bytes), Width => Width);
   end Plan;
end ACPI_PCI_Config_Reads;
