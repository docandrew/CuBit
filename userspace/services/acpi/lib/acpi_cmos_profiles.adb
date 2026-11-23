package body ACPI_CMOS_Profiles with SPARK_Mode is
   function Locate (Item : Profile; Cell : I.Byte) return Route is
   begin
      if not Supported (Item) then return (Kind => Unsupported_Device); end if;
      if Cell > Last_Cell (Item) then return (Kind => Outside_Profile); end if;
      if Cell < 128 then return (Ready, Standard_Pair, Cell); end if;
      return (Ready, Extended_Pair, Cell - 128);
   end Locate;
end ACPI_CMOS_Profiles;
