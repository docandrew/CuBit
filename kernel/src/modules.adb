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
        println;
        print ("Module name:  "); printz (strAddr); println;
        print ("Module start: "); println (modStart);
        print ("Module end:   "); println (modEnd);
        -- print (" contents :   "); println (contents);
    end printModule;

    ---------------------------------------------------------------------------
    -- isValidELF
    ---------------------------------------------------------------------------
    function isValidELF (hdr : ELF.ELFFileHeader) return Boolean with
        SPARK_Mode => On
    is
        use ELF;
    begin
        if  hdr.e_ident.EI_MAG0  /= 16#7F# or
            hdr.e_ident.EI_MAG1  /= 'E' or
            hdr.e_ident.EI_MAG2  /= 'L' or
            hdr.e_ident.EI_MAG3  /= 'F' then

            println ("Modules: Not an ELF object");
            return False;
        end if;

        if hdr.e_ident.EI_CLASS /= ELF.ELFCLASS64 then
            println ("Modules: ELF object is 32-bit, expected 64-bit");
            return False;
        end if;

        if hdr.e_ident.EI_DATA /= ELF.ELFDATA2LSB then
            println ("Modules: ELF object is MSB, expected LSB");
            return False;
        end if;

        if hdr.e_ident.EI_OSABI /= ELF.ELFOSABI_SYSV then
            println ("Modules: ELF object is not SYSV ABI");
            return False;
        end if;

        if hdr.e_type /= ELF.ET_EXEC then
            println ("Modules: ELF object is not executable");
            return False;
        end if;

        if hdr.e_machine /= ELF.EM_X86_64 then
            println ("Modules: ELF object not compiled for X86-64");
            return False;
        end if;

        return True;
    end isValidELF;

    ---------------------------------------------------------------------------
    -- printSegmentFlags
    ---------------------------------------------------------------------------
    procedure printSegmentFlags (f : in ELF.SegmentFlags) with
        SPARK_Mode => On
    is
        use ELF;
    begin

        if (f and PF_R) /= 0 then
            print ("R");
        else
            print ("-");
        end if;

        if (f and PF_W) /= 0 then
            print ("W");
        else
            print ("-");
        end if;

        if (f and PF_X) /= 0 then
            print ("X");
        else
            print ("-");
        end if;

    end printSegmentFlags;

    ---------------------------------------------------------------------------
    -- printSegmentType
    ---------------------------------------------------------------------------
    procedure printSegmentType (t : in ELF.SegmentType) with
        SPARK_Mode => On
    is
        use ELF;
    begin
        case t is
            when PT_NULL    => print ("NULL");
            when PT_LOAD    => print ("LOAD");
            when PT_DYNAMIC => print ("DYNAMIC");
            when PT_INTERP  => print ("INTERP");
            when PT_NOTE    => print ("NOTE");
            when PT_SHLIB   => print ("SHLIB");
            when PT_PHDR    => print ("PHDR");
            when others     => print ("Other");
        end case;
    end printSegmentType;

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
        if isValidELF (elfHeader) then
            println ("Modules: Found compatible, executable ELF object, checking program header...");
            print ("Modules: Program header offset:     "); println (elfHeader.e_phoff'Image);
            print ("Modules: Program header size:       "); println (elfHeader.e_phentsize);
            print ("Modules: Number of Program headers: "); println (elfHeader.e_phnum);
            print ("Modules: Entry point:               "); println (elfHeader.e_entry);

            declare
                progHeaders : ELF.ProgramHeaderTable(0..elfHeader.e_phnum) with Import, Address => modStart + elfHeader.e_phoff;
            begin
                for p of progHeaders loop
                    println;
                    print ("Modules: Segment type:            "); printSegmentType (p.p_type); println;
                    print ("Modules: Segment flags:           "); printSegmentFlags (p.p_flags); println;
                    print ("Modules: Segment offset:          "); println (p.p_offset'Image);
                    print ("Modules: Segment virtual address: "); println (p.p_vaddr);
                    print ("Modules: Segment file size:       "); println (p.p_filesz'Image);
                    print ("Modules: Segment memory size:     "); println (p.p_memsz'Image);
                    print ("Modules: Segment alignment:       "); println (p.p_align'Image);
                end loop;
            end;
        end if;
        
        println;
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