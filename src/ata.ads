-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- ATA/IDE Disk Controller Driver
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with BlockDevice;
with BufferCache;
with Filesystem.vfs;
with Spinlock;
with x86;

Pragma Elaborate_All (Spinlock);

package ata with
    SPARK_Mode => On
is

    -- Base I/O and control ports
    DEFAULT_PRIMARY_IO_BASE     : constant x86.IOPort := 16#1F0#;
    DEFAULT_PRIMARY_CTRL_BASE   : constant x86.IOPort := 16#3F4#;
    DEFAULT_SECONDARY_IO_BASE   : constant x86.IOPort := 16#170#;
    DEFAULT_SECONDARY_CTRL_BASE : constant x86.IOPort := 16#374#;

    -- Offsets from IO_BASE
    OFFSET_DATA                 : constant Unsigned_8 := 0;
    OFFSET_ERROR                : constant Unsigned_8 := 1;
    OFFSET_FEATURES             : constant Unsigned_8 := 1;
    OFFSET_SECTOR_CT            : constant Unsigned_8 := 2;
    OFFSET_SECTOR_NUM           : constant Unsigned_8 := 3;
    OFFSET_CYLINDER_LO          : constant Unsigned_8 := 4;
    OFFSET_CYLINDER_HI          : constant Unsigned_8 := 5;
    OFFSET_DRIVE_SELECT         : constant Unsigned_8 := 6;
    OFFSET_STATUS               : constant Unsigned_8 := 7;
    OFFSET_CMD                  : constant Unsigned_8 := 7;

    OFFSET_LBA_LOW              : constant Unsigned_8 := 3;
    OFFSET_LBA_MID              : constant Unsigned_8 := 4;
    OFFSET_LBA_HI               : constant Unsigned_8 := 5;

    -- Offsets from CTRL_BASE
    OFFSET_CTRL                 : constant Unsigned_8 := 0;
    OFFSET_ALT_STATUS           : constant Unsigned_8 := 0;
    OFFSET_DRIVE_ADDRESS        : constant Unsigned_8 := 1;

    -- Offsets from Bus Master base
    OFFSET_BM_CMD               : constant Unsigned_8 := 0;
    OFFSET_BM_STATUS            : constant Unsigned_8 := 2;

    -- DMA Physical Region Descriptor Table Pointer
    OFFSET_BM_PRD_TABLE_ADDR0   : constant Unsigned_8 := 4;
    OFFSET_BM_PRD_TABLE_ADDR1   : constant Unsigned_8 := 5;
    OFFSET_BM_PRD_TABLE_ADDR2   : constant Unsigned_8 := 6;
    OFFSET_BM_PRD_TABLE_ADDR3   : constant Unsigned_8 := 7;

    -- For use in the FEATURES register during packet ops
    FEATURE_DMA                 : constant Unsigned_8 := 16#01#;
    FEATURE_OVERLAPPED          : constant Unsigned_8 := 16#02#;
    FEATURE_DMADIR_WRITE        : constant Unsigned_8 := 16#04#;
    FEATURE_DMADIR_READ         : constant Unsigned_8 := 16#00#;

    -- For use in the DRIVE_SELECT command
    SELECT_MASTER               : constant Unsigned_8 := 16#A0#;
    SELECT_SLAVE                : constant Unsigned_8 := 16#B0#;
    SELECT_LBA                  : constant Unsigned_8 := 16#40#;
    SELECT_CHS                  : constant Unsigned_8 := 16#00#;

    -- Commands
    CMD_NOP                     : constant Unsigned_8 := 16#00#;
    CMD_SOFT_RESET              : constant Unsigned_8 := 16#0B#;
    
    CMD_READ_PIO                : constant Unsigned_8 := 16#20#;
    CMD_READ_PIO_RETRY          : constant Unsigned_8 := 16#21#;
    CMD_READ_PIO_EXT            : constant Unsigned_8 := 16#24#;

    CMD_READ_DMA                : constant Unsigned_8 := 16#C8#;
    CMD_READ_DMA_EXT            : constant Unsigned_8 := 16#25#;
    CMD_READ_DMA_QUEUED         : constant Unsigned_8 := 16#C7#;
    CMD_READ_DMA_QUEUED_EXT     : constant Unsigned_8 := 16#26#;
    --CMD_READ_DMA                : constant Unsigned_8 := 16#C9#;
    
    CMD_WRITE_PIO               : constant Unsigned_8 := 16#30#;
    CMD_WRITE_PIO_EXT           : constant Unsigned_8 := 16#34#;
    CMD_WRITE_DMA_EXT           : constant Unsigned_8 := 16#35#;

    CMD_WRITE_DMA_RETRY         : constant Unsigned_8 := 16#CA#;
    CMD_WRITE_DMA               : constant Unsigned_8 := 16#CB#;
    CMD_WRITE_DMA_QUEUED        : constant Unsigned_8 := 16#CC#;

    CMD_PACKET                  : constant Unsigned_8 := 16#A0#;

    CMD_FLUSH_CACHE             : constant Unsigned_8 := 16#E7#;
    CMD_FLUSH_CACHE_EXT         : constant Unsigned_8 := 16#EA#;

    CMD_IDENTIFY                : constant Unsigned_8 := 16#EC#;
    CMD_IDENTIFY_ATAPI          : constant Unsigned_8 := 16#A1#;

    -- Control
    CTRL_CLI                    : constant Unsigned_8 := 16#02#;
    CTRL_STI                    : constant Unsigned_8 := 16#00#;
    
    -- Set and then clear after 5uS to do a software reset
    CTRL_SOFT_RESET             : constant Unsigned_8 := 16#04#;
    CTRL_RESET_DONE             : constant Unsigned_8 := 16#00#;

    -- Set to read the last sent most-significant LBA48 byte
    CTRL_MS_BYTE                : constant Unsigned_8 := 16#80#;

    type ATAChannel is
    record
        ioBase                  : x86.IOPort;
        ctrlBase                : x86.IOPort;
        busMasterBase           : x86.IOPort;
        disableInterrupts       : Boolean;
    end record;

    type GeneralConfigurationWord is
    record
        res0                    : Boolean;
        isHardSectored          : Boolean;
        isSoftSectored          : Boolean;
        isNotMFMEncoded         : Boolean;
        hasHeadSwitchGT15us     : Boolean;
        hasSpindleMotorCtrl     : Boolean;
        isFixedDrive            : Boolean;
        isRemovableCartridge    : Boolean;
        rateLessThan5Mbps       : Boolean;
        rateBetween5and10Mbps   : Boolean;
        rateMoreThan10Mbps      : Boolean;
        rotSpeedTolerance       : Boolean;
        hasDataStrobeOffset     : Boolean;
        hasTrackOffset          : Boolean;
        requiresFormatSpeedGap  : Boolean;
        notATA                  : Boolean;
    end record with Size => 16;

    for GeneralConfigurationWord use
    record
        res0                    at 0 range 0..0;
        isHardSectored          at 0 range 1..1;
        isSoftSectored          at 0 range 2..2;
        isNotMFMEncoded         at 0 range 3..3;
        hasHeadSwitchGT15us     at 0 range 4..4;
        hasSpindleMotorCtrl     at 0 range 5..5;
        isFixedDrive            at 0 range 6..6;
        isRemovableCartridge    at 0 range 7..7;
        rateLessThan5Mbps       at 0 range 8..8;
        rateBetween5and10Mbps   at 0 range 9..9;
        rateMoreThan10Mbps      at 0 range 10..10;
        rotSpeedTolerance       at 0 range 11..11;
        hasDataStrobeOffset     at 0 range 12..12;
        hasTrackOffset          at 0 range 13..13;
        requiresFormatSpeedGap  at 0 range 14..14;
        notATA                  at 0 range 15..15;
    end record;

    function toGCWord is new 
        Ada.Unchecked_Conversion(Unsigned_16, GeneralConfigurationWord);

    subtype SerialString is String(1..20); -- array (Natural range 1..20) of Character;
    subtype FirmwareString is String(1..8); -- array (Natural range 1..8) of Character;
    subtype ModelString is String(1..40); -- array (Natural range 1..40) of Character;

    -- TransferInfoWord
    -- @field maxSectorsPerInterrupt, if 0x00, reserved, otherwise this is the
    --  max number of sectors that shall be transferred per interrupt on
    --  READ/WRITE MULTIPLE commands.
    -- @field vendorUnique - should be 0x80
    type TransferInfoWord is
    record
        maxSectorsPerInterrupt  : Unsigned_8;
        vendorUnique            : Unsigned_8;
    end record with Size => 16;

    for TransferInfoWord use
    record
        maxSectorsPerInterrupt  at 0 range 0..7;
        vendorUnique            at 0 range 8..15;
    end record;

    type DriveCapabilitiesWords is
    record
        retired             : Unsigned_8;
        hasDMA              : Boolean;
        hasLBA              : Boolean;
        IORDYMayBeDisabled  : Boolean;
        IORDYSupported      : Boolean;
        reservedIDPacket1   : Boolean;
        stbyTimerSupported  : Boolean;
        reservedIDPacket2   : Natural range 0..3;
        hasStbyTimerMinimum : Boolean;
        obsolete            : Boolean;
        reserved            : Natural range 0..16#7FF#;
        always1             : Boolean;
        always0             : Boolean;
    end record with Size => 32;

    for DriveCapabilitiesWords use
    record
        retired             at 0 range 0..7;
        hasDMA              at 0 range 8..8;
        hasLBA              at 0 range 9..9;
        IORDYMaybeDisabled  at 0 range 10..10;
        IORDYSupported      at 0 range 11..11;
        reservedIDPacket1   at 0 range 12..12;
        stbyTimerSupported  at 0 range 13..13;
        reservedIDPacket2   at 0 range 14..15;
        hasStbyTimerMinimum at 2 range 0..0;
        obsolete            at 2 range 1..1;
        reserved            at 2 range 2..13;
        always1             at 2 range 14..14;
        always0             at 2 range 15..15;
    end record;

    type ValidGeometryFields is
    record
        obsolete            : Boolean;
        words64to70Valid    : Boolean;
        word88Valid         : Boolean;
        reserved            : Natural range 0..16#1FFF#;
    end record with Size => 16;

    for ValidGeometryFields use
    record
        obsolete            at 0 range 0..0;
        words64to70Valid    at 0 range 1..1;
        word88Valid         at 0 range 2..2;
        reserved            at 0 range 3..15;
    end record;

    type MultiwordDMASupport is
    record
        mode0Supported      : Boolean;
        modes01Supported    : Boolean;
        modes012Supported   : Boolean;
        reserved1           : Natural range 0..31;
        mode0Selected       : Boolean;
        mode1Selected       : Boolean;
        mode2Selected       : Boolean;
        reserved2           : Natural range 0..31;
    end record with Size => 16;

    for MultiwordDMASupport use
    record
        mode0Supported      at 0 range 0..0;
        modes01Supported    at 0 range 1..1;
        modes012Supported   at 0 range 2..2;
        reserved1           at 0 range 3..7;
        mode0Selected       at 0 range 8..8;
        mode1Selected       at 0 range 9..9;
        mode2Selected       at 0 range 10..10;
        reserved2           at 0 range 11..15;
    end record;

    type MajorVersionSupported is
    record
        reserved0           : Boolean;
        obsolete1           : Boolean;
        obsolete2           : Boolean;
        obsolete3           : Boolean;
        supportsATA4        : Boolean;
        supportsATA5        : Boolean;
        supportsATA6        : Boolean;
        supportsATA7        : Boolean;
        supportsATA8        : Boolean;
        supportsATA9        : Boolean;
        supportsATA10       : Boolean;
        supportsATA11       : Boolean;
        supportsATA12       : Boolean;
        supportsATA13       : Boolean;
        supportsATA14       : Boolean;
        reserved15          : Boolean;
    end record with Size => 16;

    for MajorVersionSupported use
    record
        reserved0           at 0 range 0..0;
        obsolete1           at 0 range 1..1;
        obsolete2           at 0 range 2..2;
        obsolete3           at 0 range 3..3;
        supportsATA4        at 0 range 4..4;
        supportsATA5        at 0 range 5..5;
        supportsATA6        at 0 range 6..6;
        supportsATA7        at 0 range 7..7;
        supportsATA8        at 0 range 8..8;
        supportsATA9        at 0 range 9..9;
        supportsATA10       at 0 range 10..10;
        supportsATA11       at 0 range 11..11;
        supportsATA12       at 0 range 12..12;
        supportsATA13       at 0 range 13..13;
        supportsATA14       at 0 range 14..14;
        reserved15          at 0 range 15..15;
    end record;

    type CommandSetSupported is
    record
        hasSMART                : Boolean;
        hasSecurityMode         : Boolean;
        hasRemovableMedia       : Boolean;
        hasMandatoryPM          : Boolean;
        hasPacketCmds           : Boolean;
        hasWriteCache           : Boolean;
        hasLookAhead            : Boolean;
        hasReleaseInterrupt     : Boolean;
        hasServiceInterrupt     : Boolean;
        hasDeviceResetCmd       : Boolean;
        hasHostProtectedArea    : Boolean;
        obsolete11              : Boolean;
        hasWriteBufferCmd       : Boolean;
        hasReadBufferCmd        : Boolean;
        hasNOPCmd               : Boolean;
        obsolete15              : Boolean;
        
        hasDownloadMicrocodeCmd : Boolean;
        hasRWDmaQueued          : Boolean;
        hasCFA                  : Boolean;
        hasAPM                  : Boolean;
        hasRemovableMediaNotify : Boolean;
        hasPowerUpInStandby     : Boolean;
        setFeaturesRequired     : Boolean;
        addressOffsetResArea    : Boolean;
        hasSetMaxSecurity       : Boolean;
        hasAutoAcousticMgmt     : Boolean;
        has48BitAddress         : Boolean;
        hasDeviceCfgOverlay     : Boolean;
        hasMandatoryFlushCache  : Boolean;
        hasFlushCacheExt        : Boolean;
        always1_83              : Boolean;
        always0_83              : Boolean;

        hasSMARTErrorLogging    : Boolean;
        hasSMARTSelfTest        : Boolean;
        hasMediaSerialNumber    : Boolean;
        hasMediaCardPassThrough : Boolean;
        hasStreamingFeatures    : Boolean;
        hasGenPurposeLogging    : Boolean;
        hasWriteFUAExt          : Boolean;
        hasWriteQueuedFUAExt    : Boolean;
        has64BitWorldWideName   : Boolean;
        hasReadStreamURGBit     : Boolean;
        hasWriteStreamURGBit    : Boolean;
        reservedTechReport11    : Boolean;
        reservedTechReport12    : Boolean;
        hasIdleImmediateUnload  : Boolean;
        always1_84              : Boolean;
        always0_84              : Boolean;
    end record with Size => 48;

    for CommandSetSupported use
    record
        hasSMART                at 0 range 0..0;
        hasSecurityMode         at 0 range 1..1;
        hasRemovableMedia       at 0 range 2..2;
        hasMandatoryPM          at 0 range 3..3;
        hasPacketCmds           at 0 range 4..4;
        hasWriteCache           at 0 range 5..5;
        hasLookAhead            at 0 range 6..6;
        hasReleaseInterrupt     at 0 range 7..7;
        hasServiceInterrupt     at 0 range 8..8;
        hasDeviceResetCmd       at 0 range 9..9;
        hasHostProtectedArea    at 0 range 10..10;
        obsolete11              at 0 range 11..11;
        hasWriteBufferCmd       at 0 range 12..12;
        hasReadBufferCmd        at 0 range 13..13;
        hasNOPCmd               at 0 range 14..14;
        obsolete15              at 0 range 15..15;

        hasDownloadMicrocodeCmd at 0 range 16..16;
        hasRWDmaQueued          at 0 range 17..17;
        hasCFA                  at 0 range 18..18;
        hasAPM                  at 0 range 19..19;
        hasRemovableMediaNotify at 0 range 20..20;
        hasPowerUpInStandby     at 0 range 21..21;
        setFeaturesRequired     at 0 range 22..22;
        addressOffsetResArea    at 0 range 23..23;
        hasSetMaxSecurity       at 0 range 24..24;
        hasAutoAcousticMgmt     at 0 range 25..25;
        has48BitAddress         at 0 range 26..26;
        hasDeviceCfgOverlay     at 0 range 27..27;
        hasMandatoryFlushCache  at 0 range 28..28;
        hasFlushCacheExt        at 0 range 29..29;
        always1_83              at 0 range 30..30;
        always0_83              at 0 range 31..31;

        hasSMARTErrorLogging    at 0 range 32..32;
        hasSMARTSelfTest        at 0 range 33..33;
        hasMediaSerialNumber    at 0 range 34..34;
        hasMediaCardPassThrough at 0 range 35..35;
        hasStreamingFeatures    at 0 range 36..36;
        hasGenPurposeLogging    at 0 range 37..37;
        hasWriteFUAExt          at 0 range 38..38;
        hasWriteQueuedFUAExt    at 0 range 39..39;
        has64BitWorldWideName   at 0 range 40..40;
        hasReadStreamURGBit     at 0 range 41..41;
        hasWriteStreamURGBit    at 0 range 42..42;
        reservedTechReport11    at 0 range 43..43;
        reservedTechReport12    at 0 range 44..44;
        hasIdleImmediateUnload  at 0 range 45..45;
        always1_84              at 0 range 46..46;
        always0_84              at 0 range 47..47;
    end record;

    type CommandSetEnabled is
    record
        SMART                   : Boolean;
        securityMode            : Boolean;
        removableMedia          : Boolean;
        mandatoryPM             : Boolean;
        packetCmds              : Boolean;
        writeCache              : Boolean;
        lookAhead               : Boolean;
        releaseInterrupt        : Boolean;
        serviceInterrupt        : Boolean;
        deviceResetCmd          : Boolean;
        hostProtectedArea       : Boolean;
        obsolete11              : Boolean;
        writeBufferCmd          : Boolean;
        readBufferCmd           : Boolean;
        NOPCmd                  : Boolean;
        obsolete15              : Boolean;
        
        downloadMicrocodeCmd    : Boolean;
        RWDmaQueued             : Boolean;
        CFA                     : Boolean;
        APM                     : Boolean;
        removableMediaNotify    : Boolean;
        powerUpInStandby        : Boolean;
        setFeaturesRequired     : Boolean;
        addressOffsetResArea    : Boolean;
        setMaxSecurity          : Boolean;
        autoAcousticMgmt        : Boolean;
        enable48BitAddress      : Boolean;
        deviceCfgOverlay        : Boolean;
        flushCache              : Boolean;
        flushCacheExt           : Boolean;
        reserved86_14           : Boolean;
        reserved86_15           : Boolean;

        SMARTErrorLogging       : Boolean;
        SMARTSelfTest           : Boolean;
        mediaSerialNumber       : Boolean;
        mediaCardPassThrough    : Boolean;
        validConfigureStream    : Boolean;
        genPurposeLogging       : Boolean;
        writeFUAExt             : Boolean;
        writeQueuedFUAExt       : Boolean;
        yes64BitWorldWideName   : Boolean;
        readStreamURGBit        : Boolean;
        writeStreamURGBit       : Boolean;
        reservedTechReport11    : Boolean;
        reservedTechReport12    : Boolean;
        idleImmediateUnload     : Boolean;
        always1_84              : Boolean;
        always0_84              : Boolean;
    end record with Size => 48;

    for CommandSetEnabled use
    record
        SMART                   at 0 range 0..0;
        securityMode            at 0 range 1..1;
        removableMedia          at 0 range 2..2;
        mandatoryPM             at 0 range 3..3;
        packetCmds              at 0 range 4..4;
        writeCache              at 0 range 5..5;
        lookAhead               at 0 range 6..6;
        releaseInterrupt        at 0 range 7..7;
        serviceInterrupt        at 0 range 8..8;
        deviceResetCmd          at 0 range 9..9;
        hostProtectedArea       at 0 range 10..10;
        obsolete11              at 0 range 11..11;
        writeBufferCmd          at 0 range 12..12;
        readBufferCmd           at 0 range 13..13;
        NOPCmd                  at 0 range 14..14;
        obsolete15              at 0 range 15..15;
        
        downloadMicrocodeCmd    at 0 range 16..16;
        RWDmaQueued             at 0 range 17..17;
        CFA                     at 0 range 18..18;
        APM                     at 0 range 19..19;
        removableMediaNotify    at 0 range 20..20;
        powerUpInStandby        at 0 range 21..21;
        setFeaturesRequired     at 0 range 22..22;
        addressOffsetResArea    at 0 range 23..23;
        setMaxSecurity          at 0 range 24..24;
        autoAcousticMgmt        at 0 range 25..25;
        enable48BitAddress      at 0 range 26..26;
        deviceCfgOverlay        at 0 range 27..27;
        flushCache              at 0 range 28..28;
        flushCacheExt           at 0 range 29..29;
        reserved86_14           at 0 range 30..30;
        reserved86_15           at 0 range 31..31;

        SMARTErrorLogging       at 0 range 32..32;
        SMARTSelfTest           at 0 range 33..33;
        mediaSerialNumber       at 0 range 34..34;
        mediaCardPassThrough    at 0 range 35..35;
        validConfigureStream    at 0 range 36..36;
        genPurposeLogging       at 0 range 37..37;
        writeFUAExt             at 0 range 38..38;
        writeQueuedFUAExt       at 0 range 39..39;
        yes64BitWorldWideName   at 0 range 40..40;
        readStreamURGBit        at 0 range 41..41;
        writeStreamURGBit       at 0 range 42..42;
        reservedTechReport11    at 0 range 43..43;
        reservedTechReport12    at 0 range 44..44;
        idleImmediateUnload     at 0 range 45..45;
        always1_84              at 0 range 46..46;
        always0_84              at 0 range 47..47;
    end record;

    type UltraDMASupport is
    record
        ultraDMAMode0Supported  : Boolean;
        ultraDMAMode1Supported  : Boolean;
        ultraDMAMode2Supported  : Boolean;
        ultraDMAMode3Supported  : Boolean;
        ultraDMAMode4Supported  : Boolean;
        ultraDMAMode5Supported  : Boolean;
        ultraDMAMode6Supported  : Boolean;
        reserved7               : Boolean;
        ultraDMAMode0Selected   : Boolean;
        ultraDMAMode1Selected   : Boolean;
        ultraDMAMode2Selected   : Boolean;
        ultraDMAMode3Selected   : Boolean;
        ultraDMAMode4Selected   : Boolean;
        ultraDMAMode5Selected   : Boolean;
        ultraDMAMode6Selected   : Boolean;
        reserved15              : Boolean;
    end record with Size => 16;

    for UltraDMASupport use
    record
        ultraDMAMode0Supported  at 0 range 0..0;
        ultraDMAMode1Supported  at 0 range 1..1;
        ultraDMAMode2Supported  at 0 range 2..2;
        ultraDMAMode3Supported  at 0 range 3..3;
        ultraDMAMode4Supported  at 0 range 4..4;
        ultraDMAMode5Supported  at 0 range 5..5;
        ultraDMAMode6Supported  at 0 range 6..6;
        reserved7               at 0 range 7..7;
        ultraDMAMode0Selected   at 0 range 8..8;
        ultraDMAMode1Selected   at 0 range 9..9;
        ultraDMAMode2Selected   at 0 range 10..10;
        ultraDMAMode3Selected   at 0 range 11..11;
        ultraDMAMode4Selected   at 0 range 12..12;
        ultraDMAMode5Selected   at 0 range 13..13;
        ultraDMAMode6Selected   at 0 range 14..14;
        reserved15              at 0 range 15..15;
    end record;

    -- @param determineDeviceNum - indicates how device 0 determined the
    --  device number. 0: reserved, 1: a jumper was used, 2: CSEL was used,
    --  3: other or unknown method.
    -- @param cblidVoltage - if True, CBLID above Vih. If False, CBLID below
    --  Vil.
    type HWResetResult is
    record
        dev0_always1            : Boolean;
        dev0NumberDetermination : Natural range 0..3;
        dev0PassedDiagnostics   : Boolean;
        dev0DetectedPDIAG       : Boolean;
        dev0DetectedDASP        : Boolean;
        dev0RespondsToDev1      : Boolean;
        dev0Reserved            : Boolean;

        dev1_always1            : Boolean;
        dev1NumberDetermination : Natural range 0..3;
        dev1AssertedPDIAG       : Boolean;
        dev1Reserved            : Boolean;

        cblidVoltage            : Boolean;
        always1                 : Boolean;
        always0                 : Boolean;
    end record with Size => 16;

    for HWResetResult use
    record
        dev0_always1            at 0 range 0..0;
        dev0NumberDetermination at 0 range 1..2;
        dev0PassedDiagnostics   at 0 range 3..3;
        dev0DetectedPDIAG       at 0 range 4..4;
        dev0DetectedDASP        at 0 range 5..5;
        dev0RespondsToDev1      at 0 range 6..6;
        dev0Reserved            at 0 range 7..7;

        dev1_always1            at 0 range 8..8;
        dev1NumberDetermination at 0 range 9..10;
        dev1AssertedPDIAG       at 0 range 11..11;
        dev1Reserved            at 0 range 12..12;

        cblidVoltage            at 0 range 13..13;
        always1                 at 0 range 14..14;
        always0                 at 0 range 15..15;
    end record;

    type VendorUnique129_159Array is array (Natural range 129..159) of Unsigned_16;
    type Reserved161_175Array is array (Natural range 161..175) of Unsigned_16;
    type MediaSerialNumberArray is array (Natural range 176..205) of Unsigned_16;
    type reserved206_254Array is array (Natural range 206..254) of Unsigned_16;

    -- @field minDMAXferCycleNs - minimum multiword DMA transfer cycle time
    --  per word (nanoseconds)
    -- @field signature - if the signature is 0xA5, then the checksum is valid.
    -- @field checksum - 2s compliment sum of all bytes in words 0-254, and
    --  the first byte of word 255. If the checksum is correct, then the sum of
    --  all 512 bytes in the structure is 0.
    type DriveIdentification is
    record
        config                  : GeneralConfigurationWord; -- 0
        numCylinders            : Unsigned_16;              -- 1, obsolete
        reserved2               : Unsigned_16;              -- 2, optional
        numHeads                : Unsigned_16;              -- 3, obsolete
        bytesPerTrack           : Unsigned_16;              -- 4, retired
        bytesPerSector          : Unsigned_16;              -- 5, retired
        sectorsPerTrack         : Unsigned_16;              -- 6, obsolete
        -- words 7-8 reserved for CompactFlash
        vendorUnique0           : Unsigned_16;              -- 7, CompactFlash
        vendorUnique1           : Unsigned_16;              -- 8, CompactFlash
        vendorUnique2           : Unsigned_16;              -- 9, retired
        serialNumber            : SerialString;             -- 10-19
        bufferType              : Unsigned_16;              -- 20, retired
        bufferSize              : Unsigned_16;              -- 21, retired
        numECCBytes             : Unsigned_16;              -- 22, obsolete
        firmwareRevision        : FirmwareString;           -- 23-26
        model                   : ModelString;              -- 27-46
        transferInfo            : TransferInfoWord;         -- 47
        reserved48              : Unsigned_16;              -- 48
        capabilities            : DriveCapabilitiesWords;   -- 49-50
        vendorUnique3           : Unsigned_8;               -- 51, obsolete
        pioTimingMode           : Unsigned_8;               -- 51, obsolete
        vendorUnique4           : Unsigned_8;               -- 52, obsolete
        dmaTimingMode           : Unsigned_8;               -- 52, obsolete
        validGeometry           : ValidGeometryFields;      -- 53
        curCylinders            : Unsigned_16;              -- 54, obsolete
        curHeads                : Unsigned_16;              -- 55, obsolete
        curSectorsPerTrack      : Unsigned_16;              -- 56, obsolete
        curSectors              : Unsigned_32;              -- 57-58, obsolete
        sectorsPerInterrupt     : Unsigned_8;               -- 59
        validMultSectors        : Unsigned_8;               -- only bit 1 matters
        lbaSectors              : Unsigned_32;              -- 60-61
        hasSingleWordDMA        : Unsigned_8;               -- 62, obsolete
        singleWordDMAActive     : Unsigned_8;
        multiWordDMA            : MultiwordDMASupport;      -- 63
        pioModesSupported       : Unsigned_8;               -- 64
        reserved64Hi            : Unsigned_8;
        minDmaXferTime          : Unsigned_16;              -- 65
        reccDmaXferTime         : Unsigned_16;              -- 66
        minPIOXferTime          : Unsigned_16;              -- 67
        minPIOXferTimeIORDY     : Unsigned_16;              -- 68
        reserved69              : Unsigned_16;              -- 69
        reserved70              : Unsigned_16;              -- 70
        reservedIDPacket71      : Unsigned_16;              -- 71
        reservedIDPacket72      : Unsigned_16;              -- 72
        reservedIDPacket73      : Unsigned_16;              -- 73
        reservedIDPacket74      : Unsigned_16;              -- 74
        queueDepth              : Natural range 0..31;      -- 75
        reserved75              : Natural range 0..16#7FF#;
        reservedSATA76          : Unsigned_16;              -- 76
        reservedSATA77          : Unsigned_16;              -- 77
        reservedSATA78          : Unsigned_16;              -- 78
        reservedSATA79          : Unsigned_16;              -- 79
        majorVersion            : MajorVersionSupported;    -- 80
        minorVersion            : Unsigned_16;              -- 81
        commandSupported        : CommandSetSupported;      -- 82-84
        commandEnabled          : CommandSetEnabled;        -- 85-87
        ultraDMA                : UltraDMASupport;          -- 88
        secureEraseTime         : Unsigned_16;              -- 89
        secureEraseTimeEnh      : Unsigned_16;              -- 90
        curAPMValue             : Unsigned_16;              -- 91
        masterPasswordRev       : Unsigned_16;              -- 92
        hwReset                 : HWResetResult;            -- 93
        curAcousticMgmt         : Unsigned_8;               -- 94
        reccAcousticMgmt        : Unsigned_8;
        minStreamRequest        : Unsigned_16;              -- 95
        streamXferTimeDMA       : Unsigned_16;              -- 96
        streamLatencyDMAPIO     : Unsigned_16;              -- 97
        streamPerfGran          : Unsigned_32;              -- 98-99
        maxLBA                  : Unsigned_64;              -- 100-103
        streamXferTimePIO       : Unsigned_16;              -- 104
        reserved105             : Unsigned_16;              -- 105
        logSectorsPerPhys       : Natural range 0..15;      -- 106
        reserved106             : Unsigned_8;
        logSectorGT256Words     : Boolean;
        multLogSectorsPerPhys   : Boolean;
        always1_106             : Boolean;
        always0_106             : Boolean;
        acousticTestingDelayms  : Unsigned_16;              -- 107
        worldWideName           : Unsigned_64;              -- 108-111
        worldWideNameExt        : Unsigned_64;              -- 112-115
        reserved116             : Unsigned_16;              -- 116
        wordsPerLogicalSector   : Unsigned_32;              -- 117-118
        reserved119             : Unsigned_16;              -- 119
        reserved120             : Unsigned_16;              -- 120
        reserved121             : Unsigned_16;              -- 121
        reserved122             : Unsigned_16;              -- 122
        reserved123             : Unsigned_16;              -- 123
        reserved124             : Unsigned_16;              -- 124
        reserved125             : Unsigned_16;              -- 125
        reserved126             : Unsigned_16;              -- 126
        hasRmovableMediaNotify  : Natural range 0..3;       -- 127
        reserved127             : Natural range 0..16#3FFF#;
        securitySupported       : Boolean;                  -- 128
        securityEnabled         : Boolean;
        securityLocked          : Boolean;
        securityFrozen          : Boolean;
        securityCountExpired    : Boolean;
        hasEnhancedSecureErase  : Boolean;
        reserved128_6_7         : Natural range 0..2;
        securityLevelMax        : Boolean;
        reserved128_9_15        : Natural range 0..127;
                                                            -- 129-159
        vendorUnique129_159     : VendorUnique129_159Array; --array (range 129..159) of Unsigned_16;
        maxCurrentMilliamps     : Natural range 0..16#FFF#; -- 160
        cfaPowerMode1Disabled   : Boolean;
        cfaPowerMode1Required   : Boolean;
        reserved160             : Boolean;
        word160Supported        : Boolean;
        reserved161_175         : Reserved161_175Array; --array (range 161..175) of Unsigned_16;
        mediaSerialNum          : MediaSerialNumberArray; --array (range 176..205) of Unsigned_16;
        reserved206_254         : Reserved206_254Array; --array (range 206..254) of Unsigned_16;
        signature               : Unsigned_8;               -- 255
        checksum                : Unsigned_8;
    end record with Size => 256*16;

    Word : constant := 2;

    for DriveIdentification use
    record
        config                  at 0*Word range 0..15;
        numCylinders            at 1*Word range 0..15;
        reserved2               at 2*Word range 0..15;
        numHeads                at 3*Word range 0..15;
        bytesPerTrack           at 4*Word range 0..15;
        bytesPerSector          at 5*Word range 0..15;
        sectorsPerTrack         at 6*Word range 0..15;
        vendorUnique0           at 7*Word range 0..15;
        vendorUnique1           at 8*Word range 0..15;
        vendorUnique2           at 9*Word range 0..15;
        serialNumber            at 10*Word range 0..159;
        bufferType              at 20*Word range 0..15;
        bufferSize              at 21*Word range 0..15;
        numECCBytes             at 22*Word range 0..15;
        firmwareRevision        at 23*Word range 0..63;
        model                   at 27*Word range 0..319;
        transferInfo            at 47*Word range 0..15;
        reserved48              at 48*Word range 0..15;
        capabilities            at 49*Word range 0..31;
        vendorUnique3           at 51*Word range 0..7;
        pioTimingMode           at 51*Word range 8..15;
        vendorUnique4           at 52*Word range 0..7;
        dmaTimingMode           at 52*Word range 8..15;
        validGeometry           at 53*Word range 0..15;
        curCylinders            at 54*Word range 0..15;
        curHeads                at 55*Word range 0..15;
        curSectorsPerTrack      at 56*Word range 0..15;
        curSectors              at 57*Word range 0..31;
        sectorsPerInterrupt     at 59*Word range 0..7;
        validMultSectors        at 59*Word range 8..15;
        lbaSectors              at 60*Word range 0..31;
        hasSingleWordDMA        at 62*Word range 0..7;
        singleWordDMAActive     at 62*Word range 8..15;
        multiWordDMA            at 63*Word range 0..15;
        pioModesSupported       at 64*Word range 0..7;
        reserved64Hi            at 64*Word range 8..15;
        minDmaXferTime          at 65*Word range 0..15;
        minPIOXferTime          at 66*Word range 0..15;
        reccDmaXferTime         at 67*Word range 0..15;
        minPIOXferTimeIORDY     at 68*Word range 0..15;
        reserved69              at 69*Word range 0..15;
        reserved70              at 70*Word range 0..15;
        reservedIDPacket71      at 71*Word range 0..15;
        reservedIDPacket72      at 72*Word range 0..15;
        reservedIDPacket73      at 73*Word range 0..15;
        reservedIDPacket74      at 74*Word range 0..15;
        queueDepth              at 75*Word range 0..4;
        reserved75              at 75*Word range 5..15;
        reservedSATA76          at 76*Word range 0..15;
        reservedSATA77          at 77*Word range 0..15;
        reservedSATA78          at 78*Word range 0..15;
        reservedSATA79          at 79*Word range 0..15;
        majorVersion            at 80*Word range 0..15;
        minorVersion            at 81*Word range 0..15;
        commandSupported        at 82*Word range 0..47;
        commandEnabled          at 85*Word range 0..47;
        ultraDMA                at 88*Word range 0..15;
        secureEraseTime         at 89*Word range 0..15;
        secureEraseTimeEnh      at 90*Word range 0..15;
        curAPMValue             at 91*Word range 0..15;
        masterPasswordRev       at 92*Word range 0..15;
        hwReset                 at 93*Word range 0..15;
        curAcousticMgmt         at 94*Word range 0..7;
        reccAcousticMgmt        at 94*Word range 8..15;
        minStreamRequest        at 95*Word range 0..15;
        streamXferTimeDMA       at 96*Word range 0..15;
        streamLatencyDMAPIO     at 97*Word range 0..15;
        streamPerfGran          at 98*Word range 0..31;
        maxLBA                  at 100*Word range 0..63;
        streamXferTimePIO       at 104*Word range 0..15;
        reserved105             at 105*Word range 0..15;
        logSectorsPerPhys       at 106*Word range 0..3;
        reserved106             at 106*Word range 4..11;
        logSectorGT256Words     at 106*Word range 12..12;
        multLogSectorsPerPhys   at 106*Word range 13..13;
        always1_106             at 106*Word range 14..14;
        always0_106             at 106*Word range 15..15;
        acousticTestingDelayms  at 107*Word range 0..15;
        worldWideName           at 108*Word range 0..63;
        worldWideNameExt        at 112*Word range 0..63;
        reserved116             at 116*Word range 0..15;
        wordsPerLogicalSector   at 117*Word range 0..31;
        reserved119             at 119*Word range 0..15;
        reserved120             at 120*Word range 0..15;
        reserved121             at 121*Word range 0..15;
        reserved122             at 122*Word range 0..15;
        reserved123             at 123*Word range 0..15;
        reserved124             at 124*Word range 0..15;
        reserved125             at 125*Word range 0..15;
        reserved126             at 126*Word range 0..15;
        hasRmovableMediaNotify  at 127*Word range 0..1;
        reserved127             at 127*Word range 2..15;
        securitySupported       at 128*Word range 0..0;
        securityEnabled         at 128*Word range 1..1;
        securityLocked          at 128*Word range 2..2;
        securityFrozen          at 128*Word range 3..3;
        securityCountExpired    at 128*Word range 4..4;
        hasEnhancedSecureErase  at 128*Word range 5..5;
        reserved128_6_7         at 128*Word range 6..7;
        securityLevelMax        at 128*Word range 8..8;
        reserved128_9_15        at 128*Word range 9..15;
        vendorUnique129_159     at 129*Word range 0..495;
        maxCurrentMilliamps     at 160*Word range 0..11;
        cfaPowerMode1Disabled   at 160*Word range 12..12;
        cfaPowerMode1Required   at 160*Word range 13..13;
        reserved160             at 160*Word range 14..14;
        word160Supported        at 160*Word range 15..15;
        reserved161_175         at 161*Word range 0..239;
        mediaSerialNum          at 176*Word range 0..479;
        reserved206_254         at 206*Word range 0..783;
        signature               at 255*Word range 0..7;
        checksum                at 255*Word range 8..15;
    end record;

    BUFFER_SIZE : constant := 2048;
    type ATABuffer is array (Natural range 0..BUFFER_SIZE) of Unsigned_8;
    
    type ATAConfiguration is (MASTER, SLAVE);

    type ATAType is (PATA, ATAPI, SATA, UNKNOWN);

    ---------------------------------------------------------------------------
    -- ata.Device describes an ATA device. Each controller has up to 4 of
    --  these, a master and slave per primary and secondary channel.
    --
    -- @param physicalSectorSize - size in bytes of a physical sector. Note
    --  that logical addressing is still based on logical sectors, but physical
    --  sectors must be written to the device in a single operation. If writing
    --  a fraction of a physical sector, the device must read the entire
    --  physical sector into buffer memory, update that fraction of the
    --  physical sector, and then write the entire physical sector to the
    --  media. All write operations must begin with the first logical sector of
    --  a physical sector and end with the last logical sector of a physical
    --  sector.
    -- @param logicalSectorSize - addressable blocks on the device 
    -- @param readCommand - command used to read data from this device
    -- @param writeCommand - command used to write data to this device
    -- @TODO: it's going to be some work here to handle all the nuances of
    --  read-modify-write on AF (Advanced Format) disks. How many of these are
    --  in the wild?
    ---------------------------------------------------------------------------
    type Device is
    record
        present             : Boolean;
        config              : ATAConfiguration;
        channel             : ATAChannel;
        kind                : ATAType;
        --buffer              : ATABuffer;
        id                  : DriveIdentification;
        lock                : spinlock.SpinLock;
        physicalSectorSize  : Unsigned_32;
        logicalSectorSize   : Unsigned_32;
        readCommand         : Unsigned_8;
        writeCommand        : Unsigned_8;
    end record;

    subtype ATADriveNumber is Natural range 0..3;

    type ATADevices is array (ATADriveNumber) of Device;

    ---------------------------------------------------------------------------
    -- Array of 4 possible ATA Devices on the default controller. If additional
    --  controllers are found, they'll need an additional ATADevices
    --  object to describe its drives. (@TODO - make a separate "Host 
    --  Controller" type and place an ATADevices array as one of it's members)
    ---------------------------------------------------------------------------
    drives : ATADevices;

    -- Indices into the drives array
    PRIMARY_MASTER      : constant ATADriveNumber := 0;
    PRIMARY_SLAVE       : constant ATADriveNumber := 1;
    SECONDARY_MASTER    : constant ATADriveNumber := 2;
    SECONDARY_SLAVE     : constant ATADriveNumber := 3;

    ---------------------------------------------------------------------------
    -- Bus Mastering DMA structures
    ---------------------------------------------------------------------------
    -- If ATA is in use, we'll use 16 MiB per controller for DMA accesses,
    -- divided by the number of drives present (minus memory used for the PRDT)
    ATA_PRDT_START   : constant Integer_Address := 16#FFFF_8000_0100_0000#;
    ATA_PRDT_END     : constant Integer_Address := 16#FFFF_8000_0200_0000#;

    -- PhysicalRegionDescriptor defines a 64k region of memory used for DMA
    -- transfers.
    type PhysicalRegionDescriptor is
    record
        physBaseAddress     : Unsigned_32;
        length              : Unsigned_16;
        reserved            : Unsigned_16;
    end record with Size => 64, alignment => 4;

    for PhysicalRegionDescriptor use
    record
        physBaseAddress     at 0 range 0..31;
        length              at 4 range 0..15;
        reserved            at 6 range 0..15;
    end record;

    function u64ToPRD is new 
        Ada.Unchecked_Conversion(Unsigned_64, PhysicalRegionDescriptor);

    -- We'll use each PRD to describe a single 4k page of memory.
    type PhysicalRegionDescriptorTable is array (Natural range <>)
        of PhysicalRegionDescriptor 
        with Alignment => 4, Component_Size => 64;

    ---------------------------------------------------------------------------
    -- setupATA - perform the initial setup of this computer's ATA disk
    --  controller and identify drives connected to it.
    ---------------------------------------------------------------------------
    procedure setupATA;

    type ATADirection is (READ, WRITE);
    type ATAResult is (SUCCESS, BAD_LBA_ADDRESS, DISK_ERROR);
    --type ATATransferMode is (PIO, DMA);

    -- DMA I/O operations

    ---------------------------------------------------------------------------
    -- syncBuffer
    -- Read/Write a buffer to disk.
    ---------------------------------------------------------------------------
    procedure syncBuffer(buf : in BufferCache.BufferPtr);

    ---------------------------------------------------------------------------
    -- Access a sector on this device, synchronizing the on-disk sectors with
    --  the buffer at address buf
    -- @param drive - see ATADriveNumber
    -- @param lba - LBA address to start reading sectors from.
    -- @param numSectors - number of sectors to read.
    -- @param buf - buffer to synchronize with disk
    -- @param direction - direction of data transfer, either READ from device
    --  to host, or WRITE from host to device.
    -- @param status - result of the operation, success or some kind of
    --  failure type (see ATAResult)
    --
    -- Note - Only PIO is supported for now.
    --
    -- Limitations - only supports LBA addressing for now, disks using CHS
    --  are NOT supported.
    ---------------------------------------------------------------------------
    procedure syncBuffer(
                drive       : in out ata.Device;
                lba         : in Filesystem.vfs.LBA48;
                numSectors  : in Unsigned_32;
                buf         : in System.Address;
                direction   : in ATADirection;
                status      : out ATAResult);

end ata;
