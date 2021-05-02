-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- ELF Module Loading
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with ELF;
with TextIO; use TextIO;
with Virtmem;

package body Modules is

    ---------------------------------------------------------------------------
    -- printModuleInfo
    ---------------------------------------------------------------------------
    procedure printModule (m : in Multiboot.MBModule) with
        SPARK_Mode => On
    is
        strAddr  : System.Address := Virtmem.P2Va (Integer_Address(m.mod_string));
        modStart : System.Address := Virtmem.P2Va (Integer_Address(m.mod_start));
        modEnd   : System.Address := Virtmem.P2Va (Integer_Address(m.mod_end));
        size     : Storage_Count  := modEnd - modStart;
        contents : String(1..Natural(size)) with Import, Address => modStart;
    begin
        print ("Module name:  "); printz (strAddr);
        println;
        print ("Module start: "); println (modStart);
        print ("Module end:   "); println (modEnd);
        println;
        -- print (" contents :   "); println (contents);
    end printModule;

    ---------------------------------------------------------------------------
    -- checkELF
    ---------------------------------------------------------------------------
    procedure checkELF (hdr : ELF.ELFFileHeader) with
        SPARK_Mode => On
    is
    begin
        if hdr.e_ident.EI_MAG0 = 16#7F# and
           hdr.e_ident.EI_MAG1 = 'E' and
           hdr.e_ident.EI_MAG2 = 'L' and
           hdr.e_ident.EI_MAG3 = 'F' then
            println ("Modules: ELF object");
        else
            println ("Modules: Not an ELF object");
        end if;
    end checkELF;

    ---------------------------------------------------------------------------
    -- loadModule
    ---------------------------------------------------------------------------
    procedure loadModule (m : in Multiboot.MBModule) with
        SPARK_Mode => On
    is
        modStart  : System.Address := Virtmem.P2Va (Integer_Address(m.mod_start));
        modEnd    : System.Address := Virtmem.P2Va (Integer_Address(m.mod_end));
        size      : Storage_Count  := modEnd - modStart;
        elfHeader : ELF.ELFFileHeader with Import, Address => modStart;
    begin
        checkELF (elfHeader);
    end loadModule;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (mbinfo : in Multiboot.MultibootInfo) with
        SPARK_Mode => On
    is
    begin
        if mbinfo.flags.hasModules then
            declare
                type ModuleList is array (Unsigned_32 range 1..mbinfo.mods_count) of Multiboot.MBModule
                    with Convention => C;
                
                mods : ModuleList
                    with Import, Address => Virtmem.P2Va (Integer_Address(mbinfo.mods_addr));
            begin
                for m of mods loop
                    printModule (m);
                    loadModule (m);
                end loop;
            end;
        else
            println ("Modules: No boot drivers or services found.");
        end if;
    end setup;

end Modules;