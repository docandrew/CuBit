package body ACPI_CMOS_Indices with SPARK_Mode is
   function Resolve
     (Base, Length, Offset : Word; Last_Cell, Century : Byte;
      For_Write : Boolean) return Decision
   is
   begin
      if Length = 0 then return (Kind => Empty_Region); end if;
      if Base > Word (Last_Cell)
        or else Length - 1 > Word (Last_Cell) - Base
      then
         return (Kind => Outside_Device);
      end if;
      if Offset >= Length then return (Kind => Outside_Region); end if;
      declare
         Cell : constant Byte := Byte (Base + Offset);
      begin
         if For_Write and then Read_Only (Cell, Century) then
            return (Kind => Read_Only_Cell);
         end if;
         return (Accepted, Cell);
      end;
   end Resolve;

   function Selector (Index : Hardware_Index; NMI_Disabled : Boolean) return Byte is
     (Index or (if NMI_Disabled then 16#80# else 0));
end ACPI_CMOS_Indices;
