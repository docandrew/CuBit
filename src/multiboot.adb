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

    ---------------------------------------------------------------------------
    -- numAreas
    ---------------------------------------------------------------------------
    function numAreas (mbInfo : in MultibootInfo) return Natural is
    begin
        -- 'Size gives bits, the mmap_length is in bytes
        return Natural((mbinfo.mmap_length) / (MBMemoryArea'Size / 8));
    end numAreas;

    ---------------------------------------------------------------------------
    -- getFramebufferSize
    ---------------------------------------------------------------------------
    function getFramebufferSize (mbInfo : in MultibootInfo) return Integer_Address is
        bytesPerPixel : Unsigned_32 := Unsigned_32(mbInfo.framebuffer_bpp / 8);
    begin
        return Integer_Address(mbInfo.framebuffer_width * 
                               mbInfo.framebuffer_height * 
                               bytesPerPixel);
    end getFramebufferSize;

    ---------------------------------------------------------------------------
    -- getMemoryAreas
    -- We add an additional memory area for the framebuffer here.
    ---------------------------------------------------------------------------
    function getMemoryAreas (mbinfo : in MultibootInfo)
        return MemoryAreas.MemoryAreaArray with
        SPARK_Mode => On
    is
        upcast   : Integer_Address := Integer_Address(mbinfo.mmap_addr);
        addr     : System.Address;
        retAreas : MemoryAreas.MemoryAreaArray (1..numAreas(mbinfo) + 1);
    begin
        for i in 1..numAreas(mbinfo) loop

            addr := To_Address(upcast);
            
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
                retAreas(i).endAddr   := Integer_Address(thisArea.addr + thisArea.length - 1);

                -- advance to next area
                upcast := upcast + Integer_Address(thisArea.size + 4);
            end getThisArea;
        end loop;

        -- If there's a framebuffer, designate another memory area for it so it
        -- will be mapped appropriately in the mem manager.
        if mbInfo.flags.hasFramebuffer then
            retAreas(retAreas'Last).kind      := MemoryAreas.VIDEO;
            retAreas(retAreas'Last).startAddr := mbInfo.framebuffer_addr;
            retAreas(retAreas'Last).endAddr   := mbInfo.framebuffer_addr + getFramebufferSize(mbinfo) - 1;
        else
            -- If no framebuffer, just mark the last memory area as BAD so we
            -- don't try to do anything with it when mapping them.
            retAreas(retAreas'Last).kind := MemoryAreas.BAD;
        end if;

        return retAreas;
    end getMemoryAreas;
            
end Multiboot;
