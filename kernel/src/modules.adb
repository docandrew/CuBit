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
with Process;
with Strings;
with TextIO; use TextIO;
with Util;
with Virtmem;

package body Modules is

    ---------------------------------------------------------------------------
    -- printModuleInfo
    ---------------------------------------------------------------------------
    procedure printModule (m : in Multiboot.MBModule) with
        SPARK_Mode => On
    is
        strAddr  : constant System.Address := Virtmem.P2Va (Integer_Address(m.mod_string));
        modStart : constant System.Address := Virtmem.P2Va (Integer_Address(m.mod_start));
        modEnd   : constant System.Address := Virtmem.P2Va (Integer_Address(m.mod_end));
        size     : constant Storage_Count  := modEnd - modStart;
        contents : String(1..Natural(size)) with Import, Address => modStart;

        modName  : String(1..16);
    begin
        Strings.toAda(strAddr, modName);
        println;
        print ("Module name:  "); print(modName); println;
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
    -- toPageFlags
    -- Don't support executable data sections.
    ---------------------------------------------------------------------------
    function toPageFlags (f : ELF.SegmentFlags) return Unsigned_64 with
        SPARK_Mode => On
    is
        use type ELF.SegmentFlags;
    begin
        -- Executable
        if (f and ELF.PF_X) /= 0 and 
           (f and ELF.PF_R) /= 0 and
           (f and ELF.PF_W) = 0 then
           return Virtmem.PG_USERCODE;
        end if;

        -- Read/Write
        if (f and ELF.PF_X) = 0  and
           (f and ELF.PF_R) /= 0 and 
           (f and ELF.PF_W) /= 0 then
            return Virtmem.PG_USERDATA;
        end if;

        -- Read-only
        if (f and ELF.PF_X) = 0 and
           (f and ELF.PF_R) /= 0 and
           (f and ELF.PF_W) = 0 then
            return Virtmem.PG_USERDATARO;
        end if;

        raise ModuleException with "Modules: Bad module, invalid ELF segment flags. Segments can be only Read/Write, Read-only or Read/Executable";
    end toPageFlags;

    ---------------------------------------------------------------------------
    -- addSegmentToProcess
    ---------------------------------------------------------------------------
    procedure addSegmentToProcess (elfAddr : in System.Address;
                                   segment : in ELF.ProgramHeader;
                                   proc    : in out Process.Process) with
        SPARK_Mode => On
    is
        -- How many pages needed for this segment, and what flags?
        numPages : Storage_Count := (segment.p_memsz + Virtmem.PAGE_SIZE - 1) / Virtmem.PAGE_SIZE;
        flags    : Unsigned_64 := toPageFlags (segment.p_flags);
        storage  : System.Address;

    begin
        -- update start and end of the process' image.
        if proc.istart > segment.p_vaddr then
            proc.istart := segment.p_vaddr;
        end if;

        if proc.iend < segment.p_vaddr + segment.p_memsz then
            proc.iend := segment.p_vaddr + segment.p_memsz;
        end if;

        -- Add these page(s) to the process
        for i in 0..numPages-1 loop
            -- Add page
            print ("Modules: Mapping segment page "); print (Integer(i));
            print (" at "); println (segment.p_vaddr + (i * Virtmem.PAGE_SIZE));
            
            Process.addPage (proc    => proc,
                             mapTo   => segment.p_vaddr + (i * Virtmem.PAGE_SIZE),
                             storage => storage,
                             flags   => flags);

            -- Copy bytes from ELF image to page.
            Util.memCopy (storage, elfAddr + segment.p_offset, segment.p_filesz);

        end loop;
    end addSegmentToProcess;

    ---------------------------------------------------------------------------
    -- loadModule
    ---------------------------------------------------------------------------
    procedure loadModule (m : in Multiboot.MBModule) with
        SPARK_Mode => On
    is
        use type ELF.SegmentType;

        strAddr   : System.Address := Virtmem.P2Va (Integer_Address(m.mod_string));
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
                segments : ELF.ProgramHeaderTable(0..elfHeader.e_phnum) with Import, Address => modStart + elfHeader.e_phoff;

                newProc  : Process.Process;
                modName  : Process.ProcessName;
            begin
                Strings.toAda(strAddr, modName);

                -- Create new Process for this executable.
                newProc := Process.create (imageStart  => elfHeader.e_entry,
                                           imageSize   => size,
                                           procStart   => elfHeader.e_entry,
                                           ppid        => 0,
                                           name        => modName,
                                           priority    => 1,
                                           procStack   => To_Address(16#0000_8000_0000_0000#));

                for segment of segments loop
                    -- println;
                    -- print ("Modules: Segment type:            "); printSegmentType (segment.p_type); println;
                    -- print ("Modules: Segment flags:           "); printSegmentFlags (segment.p_flags); println;
                    -- print ("Modules: Segment offset:          "); println (segment.p_offset'Image);
                    -- print ("Modules: Segment virtual address: "); println (segment.p_vaddr);
                    -- print ("Modules: Segment file size:       "); println (segment.p_filesz'Image);
                    -- print ("Modules: Segment memory size:     "); println (segment.p_memsz'Image);
                    -- print ("Modules: Segment alignment:       "); println (segment.p_align'Image);

                    if segment.p_type = ELF.PT_LOAD and segment.p_memsz > 0 then
                        addSegmentToProcess (elfAddr => modStart,
                                             segment => segment,
                                             proc    => newProc);
                    end if;
                end loop;

                print ("Modules: Loaded module "); print (modName); print (" w/ process ID "); println (newProc.pid);
                -- @TODO this is fine for testing modules but we would rather
                -- have loadModule take a specific module name, then return the PID
                Process.addToProctab (newProc);
                Process.resume (newProc.pid);
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
