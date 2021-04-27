    ---------------------------------------------------------------------------
    -- open
    -- Determine what process is requesting this
    -- Determine the device
    -- Lookup the inode
    -- Determine permissions of the inode vs process
    -- Add it to list of open inodes
    -- Create a descriptor
    -- Add descriptor to process
    -- Return descriptor to process
    ---------------------------------------------------------------------------
    function open (filenameLen : in Unsigned_64;
                   filename    : in System.Address;
                   flags       : in Unsigned_64;
                   mode        : in Unsigned_64) return Long_Integer
    is

        drive    : Devices.DriveLetter := Devices.NODRIVE;
        device   : Devices.DeviceID;
        path     : String(1..Natural(filenameLen)) with Address => filename;
        kind     : PathType;
        idx      : Natural := 1;
        nextPos  : Natural := 1;
        
        nextInode    : Filesystem.Ext2.InodeAddr;
    begin
        print ("Open file: ");
        printz (filename);
        print (" mode: "); printdln (mode);

        if isPathAbsolute (path) then
            device := getDevice (path);
            nextInode := Process.getCurrentProcess.workingDirectory;
        else
            device := Process.getCurrentProcess.workingDevice;
            nextInode := 2;    -- '/' on Ext2.
        end if;

        if device.major = NO_MAJOR or device.minor = NO_MINOR then
            return -1;
        end if;

        -- Get filesystem of drive

        -- From the start, find next path element.
        loop
            readInode (fs, nextInode);
            exit when idx > filenameLen 
        end loop;

    end open;