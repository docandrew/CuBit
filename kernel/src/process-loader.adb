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
with Kernel_Window;
with Interrupt_Windows;
with Process_Pool;
with Process_Frames;
with Image_Shares;
with Image_Share_Policy;
with Memory_Accounting;
with Process_Memory_Budget;
with Readiness_Pages;
with Strings;
with TextIO; use TextIO;

package body Process.Loader is

    use type Image_Share_Policy.Segment_Count;

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
    -- noteBounds: the start and end of the process' image, and the start of
    -- its heap, after a segment [first, last).
    ---------------------------------------------------------------------------
    procedure noteBounds (proc : in out Process; first, last : System.Address) is
    begin
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
    end noteBounds;

    ---------------------------------------------------------------------------
    -- shareSegments (KERN-006 layer 1): map the read-only segments named by
    -- key from the image's shared frames, the first load of the image
    -- filling them. Shared False: none mapped, the loader copies every
    -- segment privately (the table or memory is full, or the bytes differ
    -- from the image already shared under this key). Failed: the load
    -- cannot go on (pages mapped meanwhile go with the unpublished process:
    -- discardUnpublished releases the share).
    ---------------------------------------------------------------------------
    procedure shareSegments (elfAddr   : System.Address;
                             key       : Image_Share_Policy.Key;
                             pid       : ProcessID;
                             sourcePID : ProcessID;
                             shared    : out Boolean;
                             failed    : out Boolean) with
        SPARK_Mode => Off -- Page tables, windows and the share table.
    is
        package Policy renames Image_Share_Policy;
        use type Image_Shares.Share;
        PAGE : constant Storage_Count := Virtmem.PAGE_SIZE;
        Words : constant := Policy.Page_Bytes / 8;
        type Page_Words is array (1 .. Words) of Unsigned_64;
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
        pages : Policy.Page_Count;
        fits, reserved, fresh, cancelled, mapped, copied, same : Boolean;
        charge : Memory_Accounting.Identity;
        S : Image_Shares.Share;
        scratch : System.Address := System.Null_Address;
        pace : Interrupt_Windows.Pace := Interrupt_Windows.Start;

        -- The file bytes of one page of segment seg (page i), and how many.
        procedure fileSpan (seg : Policy.Segment; i : Unsigned_64; count : out Storage_Count) is
            pageOff : constant Unsigned_64 := i * Policy.Page_Bytes;
        begin
            count := 0;
            if pageOff < seg.File_Bytes then
                count := Storage_Count (Unsigned_64'Min (seg.File_Bytes - pageOff, Policy.Page_Bytes));
            end if;
        end fileSpan;

        -- Fill (fresh) or compare (joined) every page; Good False on the
        -- first that cannot be read, allocated or differs.
        -- Why a load was not shared (said once per image and reason).
        why : Image_Shares.Fallback := Image_Shares.No_Room;
        procedure eachPage (Good : out Boolean) is
            index : Natural := 0;
            frame : Virtmem.PhysAddress;
            count : Storage_Count;
            window : System.Address;
            bound : Boolean;
        begin
            Good := False;
            for k in 1 .. key.Count loop
                for i in 0 .. Policy.Pages_Of (key.Segments (k)) - 1 loop
                    Interrupt_Windows.Point (pace);
                    fileSpan (key.Segments (k), i, count);
                    if fresh then
                        Process_Pool.Alloc_Frame (frame);
                        if frame = 0 then why := Image_Shares.Frame_Refused; return; end if;
                        if count > 0 then
                            Kernel_Window.Open (Process_Frames.Make (Unsigned_64 (frame)), window);
                            readSource (elfAddr, key.Segments (k).Offset + i * Policy.Page_Bytes,
                                        count, window, sourcePID, copied);
                            Kernel_Window.Close;
                            if not copied then
                                BuddyAllocator.freeFrame (frame);
                                why := Image_Shares.Source_Unreadable;
                                return;
                            end if;
                        end if;
                        Image_Shares.Set_Frame (S, index, frame, bound);
                        if not bound then
                            why := Image_Shares.Charge_Refused;
                            BuddyAllocator.freeFrame (frame);
                            return;
                        end if;
                    else
                        -- This load's bytes, beside the image's frame.
                        declare
                            mine : Page_Words with Import, Address => scratch;
                        begin
                            mine := (others => 0);
                        end;
                        if count > 0 then
                            readSource (elfAddr, key.Segments (k).Offset + i * Policy.Page_Bytes,
                                        count, scratch, sourcePID, copied);
                            if not copied then why := Image_Shares.Source_Unreadable; return; end if;
                        end if;
                        frame := Image_Shares.Frame (S, index);
                        Kernel_Window.Open (Process_Frames.Make (Unsigned_64 (frame)), window);
                        declare
                            mine : Page_Words with Import, Address => scratch;
                            theirs : Page_Words with Import, Address => window;
                        begin
                            same := mine = theirs;
                        end;
                        Kernel_Window.Close;
                        if not same then why := Image_Shares.Bytes_Differ; return; end if;
                    end if;
                    index := index + 1;
                end loop;
            end loop;
            Good := True;
        end eachPage;

        good : Boolean;
    begin
        shared := False;
        failed := False;
        Policy.Total_Pages (key, pages, fits);
        if key.Count = 0 or else not fits or else pages = 0 then
            return;
        end if;
        -- The process's own charge for the pages it maps (Image_Shares:
        -- each user pays its pages, as if private).
        Memory_Accounting.Reserve (proctab(pid).memoryAccount, Process_Memory_Budget.Ordinary,
                                   Memory_Accounting.Identity (pages), charge, reserved);
        if not reserved then
            Image_Shares.Note_Fallback (key, Image_Shares.Quota_Refused, proctab(pid).name);
            return;
        end if;
        Image_Shares.Acquire (key, pages, S, fresh);
        if S = Image_Shares.No_Share then
            Image_Shares.Note_Fallback (key, Image_Shares.No_Room, proctab(pid).name);
            Memory_Accounting.Cancel_Unbound (charge, Memory_Accounting.Identity (pages), cancelled);
            if not cancelled then raise ProcessLoadException with "image charge rollback failed"; end if;
            return;
        end if;
        if not fresh then
            BuddyAllocator.alloc (0, scratch);
        end if;
        if not fresh and then scratch = BuddyAllocator.NO_BLOCK_AVAILABLE then
            good := False;
        else
            eachPage (good);
        end if;
        if scratch /= System.Null_Address then
            BuddyAllocator.free (0, scratch);
        end if;
        if not good then
            -- Not shared after all: the share and the charge go back, and
            -- every segment is copied privately. Said once per image and
            -- reason, so a sharing regression names itself.
            Image_Shares.Note_Fallback (key, why, proctab(pid).name);
            Image_Shares.Release (S);
            Memory_Accounting.Cancel_Unbound (charge, Memory_Accounting.Identity (pages), cancelled);
            if not cancelled then raise ProcessLoadException with "image charge rollback failed"; end if;
            return;
        end if;
        if fresh then
            Image_Shares.Publish (S);
        end if;
        -- From here the process holds the share: discardUnpublished or
        -- reclaimProcess give it and the charge back.
        proctab(pid).imageShare := S;
        proctab(pid).imageCharge := charge;
        proctab(pid).imageSharedPages := Unsigned_64 (pages);
        declare
            index : Natural := 0;
        begin
            for k in 1 .. key.Count loop
                declare
                    seg : Policy.Segment renames key.Segments (k);
                    first : constant System.Address := To_Address (Integer_Address (seg.Address));
                    flags : constant Unsigned_64 := (if seg.Executable then Virtmem.PG_USERCODE
                                                     else Virtmem.PG_USERDATARO);
                begin
                    noteBounds (proctab(pid), first, first + Storage_Count (seg.Memory_Bytes));
                    for i in 0 .. Policy.Pages_Of (seg) - 1 loop
                        Interrupt_Windows.Point (pace);
                        if Virtmem.tableWalk (To_Integer (first + Storage_Count (i) * PAGE),
                                              addrtab(proctab(pid).pgTable), Allow_Big => True) /= 0
                        then
                            failed := True;
                            return;
                        end if;
                        mapPage (Image_Shares.Frame (S, index), To_Integer (first + Storage_Count (i) * PAGE),
                                 flags, addrtab(proctab(pid).pgTable), mapped);
                        if not mapped then
                            failed := True;
                            return;
                        end if;
                        index := index + 1;
                    end loop;
                end;
            end loop;
        end;
        Readiness_Pages.Set_Shared (proctab(pid).readinessFrame,
                                    Unsigned_64 (pages) * Policy.Page_Bytes);
        shared := True;
    end shareSegments;

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
        storage  : Process_Frames.Process_Frame;
        allocation : Page_Allocation_Result;
        copied : Boolean;
        first : constant System.Address := To_Address (Integer_Address (segment.p_vaddr));
        last : constant System.Address := first + Storage_Count (segment.p_memsz);
        pace : Interrupt_Windows.Pace := Interrupt_Windows.Start;

    begin
        success := False;
        noteBounds (proc, first, last);

        -- Add these page(s) to the process
        print ("Process.Loader: Mapping "); print (Integer(numPages));
        print (" pages at "); println (first);

        for i in 0..numPages-1 loop
            -- A large image takes milliseconds to copy, and a system call
            -- runs with interrupts off: no lock and no window slot is held
            -- between pages (the child is not yet published), so this CPU's
            -- interrupts get a window every Interrupt_Windows.Interval_Us.
            Interrupt_Windows.Point (pace);
            tryAddPage (proc    => proc,
                     mapTo   => first + (i * Virtmem.PAGE_SIZE),
                     frame   => storage,
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
                    -- The child's frame, through this CPU's window
                    -- (KERN-014): never through the kernel's linear map.
                    declare
                        page : System.Address;
                    begin
                        Kernel_Window.Open (storage, page);
                        readSource (elfAddr, segment.p_offset + Unsigned_64 (pageOff),
                                    copySize, page, sourcePID, copied);
                        Kernel_Window.Close;
                    end;
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
                   ppid         : ProcessID := NO_PROCESS;
                   sourcePID    : ProcessID;
                   handleLimit  : Process_Capabilities.Tables.Handle_Limit) return ProcessID with
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
                shareKey : Image_Share_Policy.Key;
                sharedImage, shareFailed : Boolean;

                -- One of the segments shareSegments mapped.
                function isShared (segment : ELF_Admission.Program_Header) return Boolean is
                  (for some k in 1 .. shareKey.Count =>
                     shareKey.Segments (k).Address = segment.p_vaddr and then
                     shareKey.Segments (k).Offset = segment.p_offset and then
                     shareKey.Segments (k).Memory_Bytes = segment.p_memsz);
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
                               procStack    => PROCESS_STACK_TOP_VIRT,
                               stackSize    => UserStackSize (requestedStackSize),
                               imageFrames  => imagePages,
                               handleLimit  => handleLimit,
                               requestedPID => requestedPID);

                if pid = NO_PROCESS then return NO_PROCESS; end if;

                -- The image's frames made clean first (laundered or grown,
                -- interrupt windows between batches), so the page loop
                -- below rarely zeroes a batch or a region in one go.
                Process_Pool.Prepare (imagePages);

                -- KERN-006: the read-only segments this image shares with
                -- other processes of the same image; the others are copied.
                for segment of segments loop
                    if segment.p_type = ELF.SegmentType'Enum_Rep (ELF.PT_LOAD) and then
                      Image_Share_Policy.Shareable (segment.p_flags, segment.p_vaddr,
                                                    segment.p_filesz, segment.p_memsz) and then
                      shareKey.Count < Image_Share_Policy.Segment_Count'Last
                    then
                        shareKey.Count := shareKey.Count + 1;
                        shareKey.Segments (shareKey.Count) :=
                          (Address      => segment.p_vaddr,
                           Offset       => segment.p_offset,
                           File_Bytes   => segment.p_filesz,
                           Memory_Bytes => segment.p_memsz,
                           Executable   => (segment.p_flags and Unsigned_32 (ELF.PF_X)) /= 0);
                    end if;
                end loop;
                shareKey.Entry_Point := Unsigned_64 (To_Integer (elfHeader.e_entry));
                shareSegments (objStart, shareKey, pid, sourcePID, sharedImage, shareFailed);
                if shareFailed then
                    discardUnpublished (pid);
                    println ("Process.Loader: shared image mapping failed");
                    return NO_PROCESS;
                end if;

                for segment of segments loop
                    if segment.p_type = ELF.SegmentType'Enum_Rep (ELF.PT_LOAD) and then segment.p_memsz > 0
                      and then not (sharedImage and then isShared (segment))
                    then
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
                   ppid : ProcessID := NO_PROCESS;
                   sourcePID : ProcessID := NO_PROCESS;
                   handleLimit : Process_Capabilities.Tables.Handle_Limit :=
                     Process_Capabilities.Tables.Default_Limit) return ProcessID
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
                                 requestedPID, ppid, sourcePID,
                                 handleLimit);
        else
            println ("Process.Loader: program-header source unreadable");
            pid := NO_PROCESS;
        end if;
        BuddyAllocator.free (order, snapshot);
        return pid;
    end load;

end Process.Loader;
