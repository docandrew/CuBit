-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Multiboot
-------------------------------------------------------------------------------
with System;

package body Multiboot
    with SPARK_Mode => On
is

    function numAreas(mbinfo : in MultibootInfo) return Natural is
    begin
        -- 'Size gives bits, the mmap_length is in bytes
        return Natural((mbinfo.mmap_length) / (MBMemoryArea'Size / 8));
    end numAreas;

    ---------------------------------------------------------------------------
    -- TODO: add 
    ---------------------------------------------------------------------------
    function getMemoryAreas(mbinfo : in MultibootInfo) 
        return MemoryAreas.MemoryAreaArray with
        SPARK_Mode => On
    is
        -- type MemoryAreaPtr is access constant MemoryArea;
        --package convert is new System.Address_To_Access_Conversions(MemoryArea);
        upcast      : Integer_Address := Integer_Address(mbinfo.mmap_addr);    
        addr        : System.Address;
        retAreas    : MemoryAreas.MemoryAreaArray (1..numAreas(mbinfo) + 1);
    begin
        for i in 1..numAreas(mbinfo) loop

            addr := To_Address(upcast);                             -- get System.Address
            
            getThisArea : declare
                thisArea : MBMemoryArea with
                    Import, Address => addr;
            begin              
                -- print("mmap: "); print(thisArea.addr);
                -- print(" size: "); print(thisArea.size);
                -- print(" length: "); print(thisArea.length);
                -- print(" kind: "); println(thisArea.kind);
                case thisArea.kind is
                    when MEMORY_USABLE =>
                        retAreas(i).kind := MemoryAreas.USABLE;
                    when MEMORY_RESERVED =>
                        retAreas(i).kind := MemoryAreas.RESERVED;
                    when MEMORY_ACPI =>
                        retAreas(i).kind := MemoryAreas.ACPI;
                    when MEMORY_HIBER =>
                        retAreas(i).kind := MemoryAreas.HIBERNATE;
                    when others =>
                        retAreas(i).kind := MemoryAreas.BAD;
                end case;

                retAreas(i).startAddr := Integer_Address(thisArea.addr);
                retAreas(i).endAddr   :=
                    Integer_Address(thisArea.addr + Unsigned_64(thisArea.length) - 1);

                -- advance to next area
                upcast := upcast + Integer_Address(thisArea.size + 4);
            end getThisArea;
        end loop;

        return retAreas;
    end getMemoryAreas;
            
end Multiboot;