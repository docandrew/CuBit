package body Platform_Power_Port_Guard with SPARK_Mode is
    function Refuses
      (D : PR.Description; P : Port; W : Width; Value : Unsigned_32) return Boolean
    is
    begin
        -- A write wrapping past the last port is refused outright.
        if P + Unsigned_32 (W) - 1 > 16#FFFF# then
            return True;
        end if;
        if P = PCI_Config_Address_Port and then W = PCI_Config_Address_Width then
            return False;
        end if;
        for I in 0 .. W - 1 loop
            declare
                Q : constant Unsigned_32 := P + Unsigned_32 (I);
                B : constant Unsigned_8 := Byte_Of (Value, I);
            begin
                if Is_IO_Port_Byte (D.Reset, Q, 0) then
                    return True;
                end if;
                if (Is_IO_Port_Byte (D.PM1A_Control, Q, PM1_SLP_EN_Byte)
                    or else Is_IO_Port_Byte (D.PM1B_Control, Q, PM1_SLP_EN_Byte))
                  and then (B and PM1_SLP_EN_Bit) /= 0
                then
                    return True;
                end if;
                if Is_IO_Port_Byte (D.Sleep_Control, Q, 0)
                  and then (B and Reduced_SLP_EN) /= 0
                then
                    return True;
                end if;
            end;
        end loop;
        return False;
    end Refuses;
end Platform_Power_Port_Guard;
