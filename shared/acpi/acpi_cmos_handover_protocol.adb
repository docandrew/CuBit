package body ACPI_CMOS_Handover_Protocol with SPARK_Mode is
   function Decode (Input : Words) return Decoded is
      Device : Profile;
   begin
      if Input (0) /= First_Port or else Input (3) not in 1 .. 3
        or else Input (2) not in 1 | 3
      then return (Valid => False); end if;
      Device := Profile'Val (Input (3));
      if Input (1) /= Port_Count (Device) then return (Valid => False); end if;
      return (True, (Device, Input (2) = 3));
   end Decode;
   function Encode (Item : Receipt) return Words is
     ([First_Port, Port_Count (Item.Device), (if Item.NMI_Disabled then 3 else 1),
       Profile'Pos (Item.Device)]);
end ACPI_CMOS_Handover_Protocol;
