pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

package body MemoryAreas is
    procedure Allocation_Map (Areas : MemoryAreaArray;
                              Result : out Firmware_Frames.Region_Array)
    is
        package FF renames Firmware_Frames;
        pragma Compile_Time_Error
          (Virtmem.FRAME_SIZE /= Integer_Address (FF.Page_Bytes) or else
           Virtmem.PhysAddress'Last > Integer_Address (FF.Byte_Address'Last),
           "Firmware admission geometry must cover the kernel physical layout");
    begin
        if Result'First /= Areas'First or else Result'Last /= Areas'Last then
            raise InvalidMemoryMap with "Firmware normalization output bounds mismatch";
        end if;
        Result := [others => <>];
        for I in Areas'Range loop
            if Areas (I) /= Empty_Area then
                if Areas (I).startAddr > Areas (I).endAddr then
                    raise InvalidMemoryMap with "Reversed firmware memory region";
                end if;
                if Areas (I).kind = USABLE then
                    Result (I) := (FF.Usable, FF.Whole_Pages
                      (FF.Byte_Address (Areas (I).startAddr),
                       FF.Byte_Address (Areas (I).endAddr)));
                else
                    Result (I) := (FF.Reserved, FF.Touched_Pages
                      (FF.Byte_Address (Areas (I).startAddr),
                       FF.Byte_Address (Areas (I).endAddr)));
                end if;
            end if;
        end loop;
    end Allocation_Map;
end MemoryAreas;
