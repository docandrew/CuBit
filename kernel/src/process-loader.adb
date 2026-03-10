-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- ELF Loading & Process Creation
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Process;
with Strings;
with TextIO; use TextIO;

package body Process.Loader is

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

            println ("Process.Loader: Not an ELF object");
            return False;
        end if;

        if hdr.e_ident.EI_CLASS /= ELF.ELFCLASS64 then
            println ("Process.Loader: ELF object is 32-bit, expected 64-bit");
            return False;
        end if;

        if hdr.e_ident.EI_DATA /= ELF.ELFDATA2LSB then
            println ("Process.Loader: ELF object is MSB, expected LSB");
            return False;
        end if;

        if hdr.e_ident.EI_OSABI /= ELF.ELFOSABI_SYSV then
            println ("Process.Loader: ELF object is not SYSV ABI");
            return False;
        end if;

        if hdr.e_type /= ELF.ET_EXEC then
            println ("Process.Loader: ELF object is not executable");
            return False;
        end if;

        if hdr.e_machine /= ELF.EM_X86_64 then
            println ("Process.Loader: ELF object not compiled for X86-64");
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

        raise ProcessLoadException with "Process.Loader: Bad module, invalid ELF segment flags. Segments can be only Read/Write, Read-only or Read/Executable";
    end toPageFlags;

    ---------------------------------------------------------------------------
    -- addSegmentToProcess
    ---------------------------------------------------------------------------
    procedure addSegmentToProcess (elfAddr : in System.Address;
                                   segment : in ELF.ProgramHeader;
                                   proc    : in out Process) with
        SPARK_Mode => On
    is
        -- How many pages needed for this segment, and what flags?
        numPages : Storage_Count := (segment.p_memsz + Virtmem.PAGE_SIZE - 1) / Virtmem.PAGE_SIZE;
        flags    : Unsigned_64 := toPageFlags (segment.p_flags);
        storage  : System.Address;

    begin
        -- update start and end of the process' image, and start of the heap
        if proc.istart > segment.p_vaddr then
            proc.istart := segment.p_vaddr;
        end if;

        if proc.iend < segment.p_vaddr + segment.p_memsz then
            proc.iend      := segment.p_vaddr + segment.p_memsz;

            -- Put a guard page between the end of the image and the heap
            -- Round down a page and then add 2 pages. Heap size = 0 to start
            proc.heapStart := To_Address ((To_Integer (proc.iend) and Virtmem.PAGE_MASK) + 
                                          (2 * Virtmem.PAGE_SIZE));
            proc.heapEnd   := proc.heapStart;
        end if;

        -- Add these page(s) to the process
        print ("Process.Loader: Mapping "); print (Integer(numPages));
        print (" pages at "); println (segment.p_vaddr);

        for i in 0..numPages-1 loop
            addPage (proc    => proc,
                     mapTo   => segment.p_vaddr + (i * Virtmem.PAGE_SIZE),
                     storage => storage,
                     flags   => flags);

            -- Copy the portion of the segment that falls within this page.
            -- Each page gets at most PAGE_SIZE bytes from the file, starting
            -- at offset p_offset + i * PAGE_SIZE within the ELF image.
            declare
                pageOff  : constant Storage_Count := i * Virtmem.PAGE_SIZE;
                copySize : Storage_Count;
            begin
                if pageOff < segment.p_filesz then
                    copySize := segment.p_filesz - pageOff;
                    if copySize > Virtmem.PAGE_SIZE then
                        copySize := Virtmem.PAGE_SIZE;
                    end if;
                    Util.memCopy (storage,
                                  elfAddr + segment.p_offset + pageOff,
                                  copySize);
                end if;
            end;

        end loop;
        println ("Process.Loader: Segment mapped OK.");
    end addSegmentToProcess;

    ---------------------------------------------------------------------------
    -- load
    ---------------------------------------------------------------------------
    function load (elfHeader    : ELF.ELFFileHeader;
                   objStart     : System.Address;
                   size         : System.Storage_Elements.Storage_Count;
                   strAddr      : System.Address;
                   requestedPID : ProcessID := NO_PROCESS;
                   priority     : ProcessPriority := 1;
                   ppid         : ProcessID := NO_PROCESS) return ProcessID with
        SPARK_Mode => On
    is
        use type ELF.SegmentType;
    begin
        if isValidELF (elfHeader) then
            println ("Process.Loader: Found compatible, executable ELF object, checking program header...");
            print ("Process.Loader: Program header offset:     "); println (elfHeader.e_phoff'Image);
            print ("Process.Loader: Program header size:       "); println (elfHeader.e_phentsize);
            print ("Process.Loader: Number of Program headers: "); println (elfHeader.e_phnum);
            print ("Process.Loader: Entry point:               "); println (elfHeader.e_entry);

            declare
                segments : ELF.ProgramHeaderTable(0..elfHeader.e_phnum - 1)
                    with Import, Address => objStart + elfHeader.e_phoff;

                pid      : ProcessID;
                procName : ProcessName;
            begin
                Strings.toAda(strAddr, procName);

                pid := create (procStart    => elfHeader.e_entry,
                               ppid         => ppid,
                               name         => procName,
                               priority     => priority,
                               procStack    => PROCESS_STACK_TOP_VIRT,
                               requestedPID => requestedPID);

                for segment of segments loop
                    if segment.p_type = ELF.PT_LOAD and segment.p_memsz > 0 then
                        addSegmentToProcess (elfAddr => objStart,
                                             segment => segment,
                                             proc    => proctab(pid));
                    end if;
                end loop;

                print ("Process.Loader: Loaded module "); print (procName);
                print (" w/ process ID "); println (pid);

                return pid;
            end;
        end if;

        return NO_PROCESS;
    end load;

end Process.Loader;