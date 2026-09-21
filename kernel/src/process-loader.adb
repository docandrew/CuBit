-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- ELF Loading & Process Creation
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Process;
with ELF_Admission;
with Process.User_Memory;
with Strings;
with TextIO; use TextIO;

package body Process.Loader is

    -- Boot modules use trusted kernel storage. Syscall images use checked,
    -- retained reads of the executing caller's readable RAM for EVERY byte
    -- read (owned pages or its explicit, permanently reserved initrd mapping).
    procedure readSource
      (Base : System.Address; Offset : Unsigned_64; Count : Storage_Count;
       Destination : System.Address; Owner : ProcessID; Success : out Boolean)
    is
        Address : constant Unsigned_64 := Unsigned_64 (To_Integer (Base));
    begin
        Success := False;
        if Owner = NO_PROCESS then
            Util.memCopy (Destination, Base + Storage_Offset (Offset), Count);
            Success := True;
        elsif Address <= Unsigned_64'Last - Offset then
            User_Memory.Copy (Owner, Address + Offset, Destination, Count, Success);
        end if;
    end readSource;

    ---------------------------------------------------------------------------
    -- isValidELF
    ---------------------------------------------------------------------------
    function isValidELF (hdr : ELF.ELFFileHeader) return Boolean with
        SPARK_Mode => Off
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
            when PT_GNU_STACK => print ("GNU_STACK");
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
                                   segment : in ELF_Admission.Program_Header;
                                   proc    : in out Process;
                                   success : out Boolean;
                                   sourcePID : ProcessID) with
        SPARK_Mode => Off -- Custom pool storage and page-table updates.
    is
        -- How many pages needed for this segment, and what flags?
        numPages : constant Storage_Count := Storage_Count
          ((segment.p_memsz + 4095) / 4096);
        flags    : constant Unsigned_64 := toPageFlags (ELF.SegmentFlags (segment.p_flags));
        storage  : System.Address;
        allocation : Page_Allocation_Result;
        copied : Boolean;
        first : constant System.Address := To_Address (Integer_Address (segment.p_vaddr));
        last : constant System.Address := first + Storage_Count (segment.p_memsz);

    begin
        success := False;
        -- update start and end of the process' image, and start of the heap
        if proc.istart > first then
            proc.istart := first;
        end if;

        if proc.iend < last then
            proc.iend      := last;

            -- Put a guard page between the end of the image and the heap
            -- Round down a page and then add 2 pages. Heap size = 0 to start
            proc.heapStart := To_Address ((To_Integer (proc.iend) and Virtmem.PAGE_MASK) + 
                                          (2 * Virtmem.PAGE_SIZE));
            proc.heapEnd   := proc.heapStart;
        end if;

        -- Add these page(s) to the process
        print ("Process.Loader: Mapping "); print (Integer(numPages));
        print (" pages at "); println (first);

        for i in 0..numPages-1 loop
            tryAddPage (proc    => proc,
                     mapTo   => first + (i * Virtmem.PAGE_SIZE),
                     storage => storage,
                     result  => allocation,
                     flags   => flags);
            if allocation /= Page_Added then
                println ("Process.Loader: segment allocation rejected");
                return;
            end if;

            -- Copy the portion of the segment that falls within this page.
            -- Each page gets at most PAGE_SIZE bytes from the file, starting
            -- at offset p_offset + i * PAGE_SIZE within the ELF image.
            declare
                pageOff  : constant Storage_Count := i * Virtmem.PAGE_SIZE;
                copySize : Storage_Count;
            begin
                if pageOff < Storage_Count (segment.p_filesz) then
                    copySize := Storage_Count (segment.p_filesz) - pageOff;
                    if copySize > Virtmem.PAGE_SIZE then
                        copySize := Virtmem.PAGE_SIZE;
                    end if;
                    readSource (elfAddr, segment.p_offset + Unsigned_64 (pageOff),
                                copySize, storage, sourcePID, copied);
                    if not copied then
                        println ("Process.Loader: source segment unreadable");
                        return;
                    end if;
                end if;
            end;

        end loop;
        println ("Process.Loader: Segment mapped OK.");
        success := True;
    end addSegmentToProcess;

    ---------------------------------------------------------------------------
    -- load
    ---------------------------------------------------------------------------
    function loadSnapshot (elfHeader    : ELF.ELFFileHeader;
                   objStart     : System.Address;
                   size         : System.Storage_Elements.Storage_Count;
                   strAddr      : System.Address;
                   headersAddr  : System.Address;
                   requestedPID : ProcessID := NO_PROCESS;
                   priority     : ProcessPriority := 1;
                   ppid         : ProcessID := NO_PROCESS;
                   sourcePID    : ProcessID := NO_PROCESS) return ProcessID with
        SPARK_Mode => Off
    is
        use type ELF.SegmentType;
        use type ELF.SegmentFlags;
    begin
        -- Private entry: load has captured a valid header and a bounded,
        -- kernel-owned program-header table before calling this builder.
            println ("Process.Loader: Found compatible, executable ELF object, checking program header...");
            print ("Process.Loader: Program header offset:     "); println (elfHeader.e_phoff'Image);
            print ("Process.Loader: Program header size:       "); println (elfHeader.e_phentsize);
            print ("Process.Loader: Number of Program headers: "); println (elfHeader.e_phnum);
            print ("Process.Loader: Entry point:               "); println (elfHeader.e_entry);

            declare
                segments : ELF_Admission.Header_Table(0..elfHeader.e_phnum - 1)
                    with Import, Address => headersAddr;

                pid                : ProcessID;
                procName           : ProcessName;
                stackHeaderCount   : Natural := 0;
                requestedStackSize : Storage_Count := 0;
                entryFound : Boolean := False;
                success : Boolean;
                imagePages : Natural := 0;
                imageLimit : Unsigned_64;
            begin
                Strings.toAda(strAddr, procName);

                -- Stack geometry is executable metadata, not ambient process
                -- state.  Require one unambiguous, non-executable declaration;
                -- CuBit intentionally has no compatibility default or ulimit-
                -- style runtime override.
                for segment of segments loop
                    if segment.p_type = ELF.SegmentType'Enum_Rep (ELF.PT_GNU_STACK) then
                        stackHeaderCount := stackHeaderCount + 1;

                        if stackHeaderCount > 1 then
                            println ("Process.Loader: ELF has multiple PT_GNU_STACK declarations");
                            return NO_PROCESS;
                        end if;

                        if segment.p_filesz /= 0 then
                            println ("Process.Loader: PT_GNU_STACK contains file data");
                            return NO_PROCESS;
                        end if;

                        if segment.p_flags /= Unsigned_32 (ELF.PF_R or ELF.PF_W)
                        then
                            println ("Process.Loader: PT_GNU_STACK is not read/write and non-executable");
                            return NO_PROCESS;
                        end if;

                        if segment.p_memsz < Unsigned_64 (MIN_USER_STACK_SIZE) or else
                           segment.p_memsz > Unsigned_64 (MAX_USER_STACK_SIZE) or else
                           segment.p_memsz mod Unsigned_64 (Virtmem.PAGE_SIZE) /= 0
                        then
                            println ("Process.Loader: PT_GNU_STACK size violates stack policy");
                            return NO_PROCESS;
                        end if;

                        requestedStackSize := Storage_Count (segment.p_memsz);
                    end if;
                end loop;

                if stackHeaderCount /= 1 then
                    println ("Process.Loader: ELF is missing required PT_GNU_STACK declaration");
                    return NO_PROCESS;
                end if;

                -- Validate every segment before reserving a PID or allocating
                -- a stack. Use subtraction-based range checks on raw integers.
                imageLimit := Unsigned_64 (Integer_Address'Min
                  (GRANT_REGION_BASE, To_Integer (PROCESS_STACK_TOP_VIRT) -
                   Integer_Address (requestedStackSize)));
                for segment of segments loop
                    if segment.p_type = ELF.SegmentType'Enum_Rep (ELF.PT_LOAD) then
                        if not ELF_Admission.Segment_Fits
                          (segment, Unsigned_64 (size), imageLimit) or else
                          (segment.p_flags /= Unsigned_32 (ELF.PF_R) and then
                           segment.p_flags /= Unsigned_32 (ELF.PF_R or ELF.PF_W) and then
                           segment.p_flags /= Unsigned_32 (ELF.PF_R or ELF.PF_X)) then
                            println ("Process.Loader: invalid load-segment geometry or flags");
                            return NO_PROCESS;
                        end if;
                        ELF_Admission.Add_Image_Pages (imagePages, segment.p_memsz, success);
                        if not success then
                            println ("Process.Loader: image frame count is not representable");
                            return NO_PROCESS;
                        end if;
                        if (segment.p_flags and Unsigned_32 (ELF.PF_X)) /= 0 and then
                          Unsigned_64 (To_Integer (elfHeader.e_entry)) >= segment.p_vaddr and then
                          Unsigned_64 (To_Integer (elfHeader.e_entry)) - segment.p_vaddr < segment.p_memsz then
                            entryFound := True;
                        end if;
                    end if;
                end loop;
                if not entryFound then
                    println ("Process.Loader: entry is outside executable segments");
                    return NO_PROCESS;
                end if;
                if ELF_Admission.Frame_Capacity
                  (imagePages, Positive (requestedStackSize / Virtmem.PAGE_SIZE), INITIAL_HEAP_FRAME_HEADROOM) = 0 then
                    println ("Process.Loader: total frame count is not representable");
                    return NO_PROCESS;
                end if;

                print ("Process.Loader: Stack reservation: ");
                printdln (Unsigned_64 (requestedStackSize));

                pid := create (procStart    => elfHeader.e_entry,
                               ppid         => ppid,
                               name         => procName,
                               priority     => priority,
                               procStack    => PROCESS_STACK_TOP_VIRT,
                               stackSize    => UserStackSize (requestedStackSize),
                               imageFrames  => imagePages,
                               requestedPID => requestedPID);

                if pid = NO_PROCESS then return NO_PROCESS; end if;

                for segment of segments loop
                    if segment.p_type = ELF.SegmentType'Enum_Rep (ELF.PT_LOAD) and segment.p_memsz > 0 then
                        addSegmentToProcess (elfAddr => objStart,
                                             segment => segment,
                                             proc    => proctab(pid),
                                             success => success,
                                             sourcePID => sourcePID);
                        if not success then
                            discardUnpublished (pid);
                            print ("Process.Loader: unpublished process reclaimed PID ");
                            println (pid);
                            return NO_PROCESS;
                        end if;
                    end if;
                end loop;

                print ("Process.Loader: Loaded module "); print (procName);
                print (" w/ process ID "); println (pid);

                publish (pid);
                return pid;
            end;
    end loadSnapshot;

    function load (elfHeader : ELF.ELFFileHeader;
                   objStart : System.Address;
                   size : System.Storage_Elements.Storage_Count;
                   strAddr : System.Address;
                   requestedPID : ProcessID := NO_PROCESS;
                   priority : ProcessPriority := 1;
                   ppid : ProcessID := NO_PROCESS;
                   sourcePID : ProcessID := NO_PROCESS) return ProcessID
      with SPARK_Mode => Off
    is
        header : constant ELF.ELFFileHeader := elfHeader;
        order : constant BuddyAllocator.Order := BuddyAllocator.getOrder
          (Storage_Count (ELF_Admission.Max_Headers * ELF_Admission.Header_Size));
        snapshot : System.Address;
        pid : ProcessID;
        copied : Boolean;
    begin
        if not isValidELF (header) then return NO_PROCESS; end if;
        if not ELF_Admission.Table_Fits
          (Unsigned_64 (size), Unsigned_64 (header.e_phoff),
           Unsigned_64 (header.e_phnum), Unsigned_64 (header.e_phentsize)) then
            println ("Process.Loader: invalid program-header table");
            return NO_PROCESS;
        end if;
        BuddyAllocator.alloc (order, snapshot);
        if snapshot = System.Null_Address then return NO_PROCESS; end if;
        -- Do not validate live metadata and then reread it during allocation:
        -- a writable shared source could change geometry between those steps.
        -- The bounded header snapshot is not placed on a 4 KiB kernel stack.
        readSource (objStart, Unsigned_64 (header.e_phoff),
          Storage_Count (header.e_phnum) * Storage_Count (ELF_Admission.Header_Size),
          snapshot, sourcePID, copied);
        if copied then
            pid := loadSnapshot (header, objStart, size, strAddr, snapshot,
                                 requestedPID, priority, ppid, sourcePID);
        else
            println ("Process.Loader: program-header source unreadable");
            pid := NO_PROCESS;
        end if;
        BuddyAllocator.free (order, snapshot);
        return pid;
    end load;

end Process.Loader;
