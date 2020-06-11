-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- ATA/IDE Disk Controller Driver
-------------------------------------------------------------------------------
with BlockDevice;
with PCI;
with Textmode; use Textmode;
with Time;
with Util;

package body ATA with
    SPARK_Mode => On
is
    -- Error register bits
    type ErrorRegister is
    record
        AMNF  : Boolean;  -- Address mark not found
        TKZNF : Boolean;  -- Track zero not found
        ABRT  : Boolean;  -- Aborted command
        MCR   : Boolean;  -- Media change request
        IDNF  : Boolean;  -- ID Not found
        MC    : Boolean;  -- Media changed
        UNC   : Boolean;  -- Uncorrectable data error
        BBK   : Boolean;  -- Bad block detected
    end record with Size => 8;

    for ErrorRegister use
    record
        AMNF  at 0 range 0..0;
        TKZNF at 0 range 1..1;
        ABRT  at 0 range 2..2;
        MCR   at 0 range 3..3;
        IDNF  at 0 range 4..4;
        MC    at 0 range 5..5;
        UNC   at 0 range 6..6;
        BBK   at 0 range 7..7;
    end record;

    function toErrReg is new 
        Ada.Unchecked_Conversion(Unsigned_8, ErrorRegister);

    -- Drive/Head Register
    type DriveSelectRegister is
    record
        BlockBits24_27  : Natural range 0..15;  -- bits 24-27 of the block number
        DRV             : Boolean;              -- Select the drive number
        res1            : Boolean;              -- always 1
        LBA             : Boolean;              -- use LBA if true
        res2            : Boolean;
    end record with Size => 8;

    for DriveSelectRegister use
    record
        BlockBits24_27  at 0 range 0..3;
        DRV             at 0 range 4..4;
        res1            at 0 range 5..5;
        LBA             at 0 range 6..6;
        res2            at 0 range 7..7;
    end record;

    function toDSReg is new 
        Ada.Unchecked_Conversion(Unsigned_8, DriveSelectRegister);

    -- Status Register bits
    type StatusRegister is
    record
        ERR  : Boolean; -- error occurred
        IDX  : Boolean; -- index, always 0
        CORR : Boolean; -- corrected data, always 0
        DRQ  : Boolean; -- Drive has PIO data to send/recv
        SRV  : Boolean; -- overlapped mode service request
        DF   : Boolean; -- Drive fault error (ERR not set)
        RDY  : Boolean; -- Clear if drive spun down or after error
        BSY  : Boolean; -- Set if drive preparing to send/recv
    end record with Size => 8;

    for StatusRegister use
    record
        ERR  at 0 range 0..0;
        IDX  at 0 range 1..1;
        CORR at 0 range 2..2;
        DRQ  at 0 range 3..3;
        SRV  at 0 range 4..4;
        DF   at 0 range 5..5;
        RDY  at 0 range 6..6;
        BSY  at 0 range 7..7;
    end record;

    function toStatReg is new
        Ada.Unchecked_Conversion(Unsigned_8, StatusRegister);

        
    -- Print a string stored in 16-bit words
    procedure printSwizzled(str : in String) with
        SPARK_Mode => On,
        Pre => str'Length mod 2 = 0 and str'Length > 1
    is
        c : Natural := str'First + 1;
    begin
        -- print 1, 0, 3, 2, 5, 4
        while True loop
            print(str(c)); print(str(c-1));
            c := c + 2;
            exit when c > str'Last;
        end loop;        
    end printSwizzled;

    --------------------------------------------------------------------------
    -- checksumATAID - ensure all bytes of the ATA ID table sum to 0
    -- @TODO - maybe put this and checksumACPI in util since they do the same
    --  thing.
    --------------------------------------------------------------------------
    function checksumATAID(drive : in out ATADrive)
        return Boolean with SPARK_Mode => Off  -- Storage_Array
    is
        tableBytes : Storage_Array(1..Storage_Offset(512))
            with Import, Address => drive.id'Address;
        sum : Unsigned_32 := 0;
    begin
        for i in 1..512 loop
            sum := sum + Unsigned_32(tableBytes(Storage_Offset(i)));
        end loop;

        return sum mod 16#100# = 0;
    end checksumATAID;

    -- Read 128 dwords (512 bytes for full drive identification)
    procedure readID(basePort : in x86.IOPort; drive : in out ATADrive) with
        SPARK_Mode => Off   -- 'Address
    is
        use x86;
    begin
        x86.ins32(basePort + OFFSET_DATA, drive.id'Address, 128);
    end readID;

    ---------------------------------------------------------------------------
    -- waitReady waits for a drive to be ready for reads/writes
    -- @return True if drive is OK, False if an error occured.
    ---------------------------------------------------------------------------
    function waitReady(drive : in ATADrive) return Boolean with
        SPARK_Mode => On
    is
        basePort : constant x86.IOPort := drive.channel.ioBase;
        driveStatus : Unsigned_8;
        statusReg : StatusRegister;
        
        use x86;        -- for "+" operator
    begin

        while True loop
            x86.in8(basePort + OFFSET_STATUS, driveStatus);
            statusReg := toStatReg(driveStatus);
            --print("drive status: "); println(driveStatus);
            if (not statusReg.BSY) and statusReg.RDY then
                exit;
            end if;
        end loop;

        if statusReg.DF or statusReg.ERR then
            return False;
        else
            return True;
        end if;
    end waitReady;

    ---------------------------------------------------------------------------
    -- waitForCommandToFinish is used to poll for the results of a drive
    --  operation.
    ---------------------------------------------------------------------------
    function waitForCommandToFinish(drive : in ATADrive) return Boolean with
        SPARK_Mode => On
    is
        ctrlBase : constant x86.IOPort := drive.channel.ctrlBase;
        ignore : Unsigned_8;

        use x86;        -- for "+" operator
    begin
        -- Delay 400ns by reading alternate status register
        x86.in8(ctrlBase + OFFSET_ALT_STATUS, ignore);
        x86.in8(ctrlBase + OFFSET_ALT_STATUS, ignore);
        x86.in8(ctrlBase + OFFSET_ALT_STATUS, ignore);
        x86.in8(ctrlBase + OFFSET_ALT_STATUS, ignore);

        return waitReady(drive);
    end waitForCommandToFinish;


    procedure identify(drive : in out ATADrive) with
        SPARK_Mode => On
    is
        
        basePort    : constant x86.IOPort := drive.channel.ioBase;
        driveStatus : Unsigned_8;
        lbaMid      : Unsigned_8;
        lbaHi       : Unsigned_8;
        statusReg   : StatusRegister;
        --waitError   : Boolean;
        isATA       : Boolean;
        --masterSlave : Unsigned_8;

        use x86;    -- for "+" operator on IOPort
    begin
        -- Select the drive
        if drive.config = MASTER then
            x86.out8(basePort + OFFSET_DRIVE_SELECT, SELECT_MASTER);
        else
            x86.out8(basePort + OFFSET_DRIVE_SELECT, SELECT_SLAVE);
        end if;

        time.sleep(1 * time.Milliseconds);

        x86.out8(basePort + OFFSET_SECTOR_CT, 0);
        x86.out8(basePort + OFFSET_LBA_LOW, 0);
        x86.out8(basePort + OFFSET_LBA_MID, 0);
        x86.out8(basePort + OFFSET_LBA_HI, 0);
        x86.out8(basePort + OFFSET_CMD, CMD_IDENTIFY);

        time.sleep(1 * time.Milliseconds);

        x86.in8(basePort + OFFSET_STATUS, driveStatus);
        --print("Drive status: "); println(driveStatus);
        
        -- per OSDev: if value is 0, drive does not exist. For other values,
        -- poll status port until BSY bit clears. Check LBAmid and LBAhi ports
        -- to see if they are non-zero. If so, the drive is not ATA.
        if driveStatus = 0 then 
            drive.present := False;
            return;
        else
            drive.present := True;

            statusLoop: while True loop
                statusReg := toStatReg(driveStatus);

                -- If error bit set, then this device is not an ATA drive,
                -- but may be an ATAPI device.
                if (not statusReg.BSY) and statusReg.DRQ then
                    isATA := True;
                    exit statusLoop;
                end if;

                if statusReg.ERR then
                    -- ATAPI and SATA will immediately report an error.
                    isATA := False;
                    exit statusLoop;
                end if;

                x86.in8(basePort + OFFSET_STATUS, driveStatus);
                --println("Drive status 2: "); println(driveStatus);
            end loop statusLoop;

            if not isATA then
                x86.in8(basePort + OFFSET_LBA_MID, lbaMid);
                x86.in8(basePort + OFFSET_LBA_HI, lbaHi);

                if lbaMid = 0 and lbaHi = 0 then
                    println("WARNING: Unrecognized Parallel ATA Drive");
                    drive.kind := UNKNOWN;
                elsif lbaMid = 16#14# and lbaHi = 16#EB# then
                    println("Found Parallel ATAPI drive");
                    drive.kind := ATAPI;
                    x86.out8(basePort + OFFSET_CMD, CMD_IDENTIFY_ATAPI);
                    -- TODO: set up CDROM or whatever
                elsif lbaMid = 16#69# and lbaHi = 16#96# then
                    println("Found Serial ATAPI drive");
                    drive.kind := ATAPI;
                    x86.out8(basePort + OFFSET_CMD, CMD_IDENTIFY_ATAPI);
                    -- TODO: write SATAPI driver
                elsif lbaMid = 16#3C# and lbaHi = 16#C3# then
                    println("Found Serial ATA drive");
                    drive.kind := SATA;
                    -- TODO: write SATA driver
                else
                    --println("Drive not present or unrecognized.");
                    drive.kind := UNKNOWN;
                    drive.present := False;
                end if;
            else
                println("Found PATA Drive");
                drive.kind := PATA;
            end if;

            --println("Reading drive identification");
            time.sleep(1 * time.Milliseconds);
            
            -- Read the drive's identification space, 512 bytes
            readID(basePort, drive);
        end if;
    end identify;

    ---------------------------------------------------------------------------
    -- finishDriveIdentification
    --  This determines sector size and drive capacity, and prints out
    --  information about the drive.
    ---------------------------------------------------------------------------
    procedure finishDriveIdentification(drive : in out ATADrive) with
        SPARK_Mode => On
    is
        physicalSectorSizeMultiplier : Unsigned_16 := 1;
    begin
        if not checksumATAID(drive) then
            println(" WARNING: Bad drive identification checksum.",
                textmode.YELLOW, textmode.BLACK);
        end if;

        print(" Model:    "); printSwizzled(drive.id.model);
        println;
        print(" Serial #: "); printSwizzled(drive.id.serialNumber);
        println;

        -- Newer drives can use advanced format with larger
        -- physical sectors, figure out the sector sizes used here.
        if drive.id.wordsPerLogicalSector /= 0 then
            drive.logicalSectorSize := 
                drive.id.wordsPerLogicalSector * 2;
        else
            drive.logicalSectorSize := 512;
        end if;

        if drive.id.multLogSectorsPerPhys then
            physicalSectorSizeMultiplier := 
                Shift_Left(1, drive.id.logSectorsPerPhys);
        end if;

        drive.physicalSectorSize := 
            Unsigned_32(physicalSectorSizeMultiplier) * drive.logicalSectorSize;
        
        --print(" Physical Sector Size 2^x: "); 
        --    println(Unsigned_32(physicalSectorSizeMultiplier));
        --print(" Logical Sector Size: "); 
        --    println(drive.logicalSectorSize);
        --print(" Physical Sector Size: ");
        --    println(drive.physicalSectorSize);
        
        print(" Capacity: ");

        if drive.id.commandSupported.has48BitAddress then
            printd(drive.id.maxLBA * Unsigned_64(drive.logicalSectorSize) / 
                16#100_000#); println(" MiB");
        else
            printd(drive.id.lbaSectors * drive.logicalSectorSize /
                16#100_000#); println(" MiB");
        end if;

        -- Set the read & write commands to be used for this drive
        -- @TODO: add DMA, other modes based on what this drive actually
        -- supports. For now, just assume LBA48
        drive.readCommand := CMD_READ_PIO_EXT;
        drive.writeCommand := CMD_WRITE_PIO_EXT;
    end finishDriveIdentification;


    procedure setupATA with
        SPARK_Mode => On
    is
        numIDE  : Natural := 0;
        bus     : PCI.PCIBusNum;
        slot    : PCI.PCISlotNum;
        func    : PCI.PCIFunctionNum;
        config  : PCI.PCIDeviceHeader;

        --primaryChannelDrives : DrivesPresent;
        --secondaryChannelDrives : DrivesPresent;
        primaryChannel : ATAChannel;
        secondaryChannel : ATAChannel;
    begin
        numIDE := PCI.getNumDevices(pci.CLASS_STORAGE_IDE);
        if numIDE = 0 then
            println("No IDE controller present.");
            
            for drive of drives loop
                drive.present := False;
            end loop;

            return;
        else
            PCI.findDevice(PCI.CLASS_STORAGE_IDE, bus, slot, func);
            print("IDE controller at ");
            print("bus: "); print(bus);
            print(" slot: "); print(slot);
            print(" func: "); print(func);
            println;

            println("Registering ATA Block Driver");
            BlockDevice.registerBlockDriver(Devices.ATA, ATA.syncBuffer'Access);

            config := pci.getDeviceConfiguration(bus, slot, func);

            -- whether or not default I/O ports are being used depends on the
            -- PCI programming/interface byte.
            if util.isBitSet(config.progInterface, 0) then
                println(" Primary ATA channel is in native-PCI mode.");
            else
                println(" Primary ATA channel in compatibility mode.");
            end if;

            if util.isBitSet(config.progInterface, 1) then
                println(" Primary ATA channel does not support other modes.");
            else
                println(" Primary ATA channel supports other modes.");
            end if;

            if util.isBitSet(config.progInterface, 2) then
                println(" Secondary ATA channel is in native-PCI mode.");
            else
                println(" Secondary ATA channel in compatibility mode.");
            end if;

            if util.isBitSet(config.progInterface, 3) then
                println(" Secondary ATA channel does not support other modes.");
            else
                println(" Secondary ATA channel supports other modes.");
            end if;

            -- Base address registers 0-3 should have bit 0 hard-wired to 1 to 
            --  indicate I/O space.
            -- BAR 0 - Base address for I/O (command block) for ATA Channel X
            if config.baseAddr0 = 0 or config.baseAddr0 = 1 then
                primaryChannel.ioBase := DEFAULT_PRIMARY_IO_BASE;
            else
                primaryChannel.ioBase := 
                    x86.IOPort(config.baseAddr0 and 16#FFFE#);
            end if;

            -- BAR 1 - Base address for control register for ATA Channel X
            if config.baseAddr1 = 0 or config.baseAddr1 = 1 then
                primaryChannel.ctrlBase := DEFAULT_PRIMARY_CTRL_BASE;
            else
                primaryChannel.ctrlBase := 
                    x86.IOPort(config.baseAddr1 and 16#FFFE#);
            end if;

            -- BAR 2 - Base address for I/O (command block) for ATA Channel Y
            if config.baseAddr2 = 0 or config.baseAddr2 = 1 then
                secondaryChannel.ioBase := DEFAULT_SECONDARY_IO_BASE;
            else
                secondaryChannel.ioBase := 
                    x86.IOPort(config.baseAddr2 and 16#FFFE#);
            end if;

            -- BAR 3 - Base address for control register for ATA Channel Y
            if config.baseAddr3 = 0 or config.baseAddr3 = 1 then
                secondaryChannel.ctrlBase := DEFAULT_SECONDARY_CTRL_BASE;
            else
                secondaryChannel.ctrlBase := 
                    x86.IOPort(config.baseAddr3 and 16#FFFE#);
            end if;

            -- BAR 4 - Base address of the ATA Bus Master I/O registers
            primaryChannel.busMasterBase := 
                x86.IOPort(config.baseAddr4 and 16#FFFE#);
            secondaryChannel.busMasterBase := primaryChannel.busMasterBase + 8;
            
            drives(PRIMARY_MASTER).channel      := primaryChannel;
            drives(PRIMARY_MASTER).config       := MASTER;

            drives(PRIMARY_SLAVE).channel       := primaryChannel;
            drives(PRIMARY_SLAVE).config        := SLAVE;
            
            drives(SECONDARY_MASTER).channel    := secondaryChannel;
            drives(SECONDARY_MASTER).config     := MASTER;
            
            drives(SECONDARY_SLAVE).channel     := secondaryChannel;
            drives(SECONDARY_SLAVE).config      := SLAVE;

            -- Disable interrupts
            x86.out8(primaryChannel.ctrlBase, CTRL_CLI);
            x86.out8(secondaryChannel.ctrlBase, CTRL_CLI);

            for i in ATADriveNumber'Range loop
                println;
                case i is
                    when PRIMARY_MASTER =>
                        println("ATA Primary Master:");
                    when PRIMARY_SLAVE =>
                        println("ATA Primary Slave:");
                    when SECONDARY_MASTER =>
                        println("ATA Secondary Master:");
                    when SECONDARY_SLAVE =>
                        println("ATA Secondary Slave:");
                end case;

                identify(drives(i));
                
                if(drives(i).present) then
                    finishDriveIdentification(drives(i));
                else
                    println("No ATA drive connected.");
                end if;
            end loop;

            println;
        end if;
    end setupATA;


    -- If buffer is dirty, write it to disk, set valid once complete.
    -- Otherwise, if it's not valid, then read it.
    procedure syncBuffer(buf : in out FileCache.BufferPtr) with SPARK_Mode => On
    is
        package VFS renames Filesystem.VFS;
        --@TODO: when we get more than one IDE controller and support
        -- partitions, we'll need to identify those via minor number as well.
        -- For now, just assume minor = drive
        drive   : constant ATADriveNumber := ATADriveNumber(buf.device.minor);
        lba     : constant VFS.LBA48 := VFS.LBA48(buf.blockNum);
        --dir     : constant ATADirection;
        res     : ATAResult;

        -- We'll read in a page-sized chunk at a time.
        sectors : constant Unsigned_32 := Virtmem.PAGE_SIZE / drives(drive).physicalSectorSize;
    begin
        --enterCriticalSection(drives(drive).lock);
        --@TODO: error checking for valid, dirty blocks, etc.
        -- println("syncBuffer: ");
        -- print("lba: "); println(lba);
        -- print("num sectors: "); println(sectors);

        if buf.dirty then
            syncBufferHelper(drives(drive), lba, sectors, buf.data, WRITE, res);
            buf.dirty := False;
            buf.valid := True;
        elsif not buf.valid then
            syncBufferHelper(drives(drive), lba, sectors, buf.data, READ, res);
            buf.valid := True;
        end if;

        --@TODO check err and signal process
        --Once async I/O is supported:
        --Process.wait(buf.all'Address, drives(drive).lock);
        
        --exitCriticalSection(drives(drive).lock);
    end syncBuffer;


    procedure syncBufferHelper(
                drive       : in out ATADrive;
                lba         : in Filesystem.VFS.LBA48;
                numSectors  : in Unsigned_32;
                buf         : in Virtmem.PhysAddress;
                direction   : in ATADirection;
                status      : out ATAResult)
    with
        SPARK_Mode => On
    is
        -- SECTOR_SIZE_WORDS : constant Unsigned_32 := 256;
        -- SECTOR_SIZE_BYTES : constant Unsigned_32 := 512;

        waitResult  : Boolean;
        selection   : Unsigned_8 := SELECT_LBA;
        baseAddr    : constant Integer_Address := buf;
        --ataCmd      : Unsigned_8 := getATACmd(drive, direction);
        use x86;
    begin

        Spinlock.enterCriticalSection(drive.lock);

        -- wait for drive to become un-busy
        waitResult := waitReady(drive);

        if not waitResult then
            status := DISK_ERROR;
            return;
        end if;
        
        -- Select the drive on the controller
        if drive.config = MASTER then
            selection := selection or SELECT_MASTER;
            --println(" selecting master");
        else
            selection := selection or SELECT_SLAVE;
            --println(" selecting slave");
        end if;

        x86.out8(drive.channel.ioBase + OFFSET_DRIVE_SELECT, selection);

        -- Set the sector count and starting LBA number        
        x86.out8(drive.channel.ioBase + OFFSET_SECTOR_CT, 
            util.getByte(numSectors, 1));   -- sector ct high byte
        --print(" num sector hi byte: "); println(util.getByte(numSectors, 1));
        
        x86.out8(drive.channel.ioBase + OFFSET_LBA_LOW, util.getByte(lba, 3));
        x86.out8(drive.channel.ioBase + OFFSET_LBA_MID, util.getByte(lba, 4));
        x86.out8(drive.channel.ioBase + OFFSET_LBA_HI, util.getByte(lba, 5));

        x86.out8(drive.channel.ioBase + OFFSET_SECTOR_CT,
            util.getByte(numSectors, 0));   -- sector ct low byte
        --print(" num sector low byte: "); println(util.getByte(numSectors, 0));
        
        x86.out8(drive.channel.ioBase + OFFSET_LBA_LOW, util.getByte(lba, 0));
        x86.out8(drive.channel.ioBase + OFFSET_LBA_MID, util.getByte(lba, 1));
        x86.out8(drive.channel.ioBase + OFFSET_LBA_HI, util.getByte(lba, 2));

        -- Send the command for the type of drive transfer to perform
        if direction = READ then
            x86.out8(drive.channel.ioBase + OFFSET_CMD, drive.readCommand);
            --print(" sending command: "); println(drive.readCommand);
        else
            x86.out8(drive.channel.ioBase + OFFSET_CMD, drive.writeCommand);
            --print(" sending command: "); println(drive.writeCommand);
        end if;


        if direction = READ then
            for i in 0 .. numSectors - 1 loop
                waitResult := waitForCommandToFinish(drive);

                if not waitResult then
                    status := DISK_ERROR;
                    return;
                end if;

                -- make sure drive didn't have an error, read an entire sector
                --  one dword at a time.
                x86.ins16(port  => drive.channel.ioBase + OFFSET_DATA, 
                          addr  => To_Address(buf + Integer_Address(i * drive.physicalSectorSize)),
                          count => drive.physicalSectorSize / 2);

                -- print(" reading sector "); print(Unsigned_32(lba) + i);
                -- print(" to ");
                -- println(To_Address(buf + Integer_Address(i * drive.physicalSectorSize)));
            end loop;
        else
            for i in 0 .. numSectors - 1 loop
                waitResult := waitForCommandToFinish(drive);

                if not waitResult then
                    status := DISK_ERROR;
                    return;
                end if;

                x86.outs16(port     => drive.channel.ioBase + OFFSET_DATA,
                           addr     => To_Address(buf + Integer_Address(i * drive.physicalSectorSize)),
                           count    => drive.physicalSectorSize / 2);
            end loop;

            x86.out8(drive.channel.ioBase + OFFSET_CMD, CMD_FLUSH_CACHE_EXT);
            waitResult := waitForCommandToFinish(drive);
        end if;

        if not waitResult then
            status := DISK_ERROR;
            return;
        end if;

        -- TODO: DMA
        -- For DMA, ???
        --  Wait for interrupt to finish up.

        status := SUCCESS;

        Spinlock.exitCriticalSection(drive.lock);
    end syncBufferHelper;

end ATA;
