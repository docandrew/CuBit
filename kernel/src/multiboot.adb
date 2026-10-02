-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
-- Multiboot loader adapter. Byte decoding is in Multiboot_Memory_Map.
-------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with TextIO; use TextIO;
with Virtmem;
with Multiboot_Memory_Map;
with Multiboot_Memory_Map.Reclaim;
with Multiboot_Entry;
with CPUID;
with Boot_Font;
with Multiboot2_Info;

package body Multiboot with SPARK_Mode => On is
    use type Boot_Framebuffer.Status;
    use type Boot_Framebuffer.Address;
    pragma Compile_Time_Error
      (Boot_Modules.Window_End /= Virtmem.BOOTSTRAP_PHYSICAL_LIMIT or else
       Boot_Modules.Page_Bytes /= Virtmem.FRAME_SIZE or else
       Boot_Framebuffer.Page_Bytes /= Virtmem.FRAME_SIZE,
       "Boot module geometry must match bootstrap mappings");
    Module_Bytes : constant Integer_Address := MBModule'Size / System.Storage_Unit;
    pragma Compile_Time_Error (MBModule'Object_Size /= MBModule'Size,
                              "Module descriptor overlay must not include padding");

    function Overlaps_Kernel (Base, Length : Integer_Address) return Boolean
      with SPARK_Mode => Off
    is
    begin
        return Base < Virtmem.V2P (To_Address (Virtmem.STACK_TOP)) and then
          Base + Length > Config.MIN_PHYS_ALLOC;
    end Overlaps_Kernel;
    -- Single-threaded boot workspace. Neither the raw length nor record count
    -- may allocate a large primary/secondary-stack object.
    bootMapSnapshot : Multiboot_Memory_Map.Entries
      (1 .. Config.MAX_BOOT_MEMORY_REGIONS);
    bootCatalog : Boot_Modules.Catalog;
    bootSnapshotReady : Boolean := False;
    bootInfoPhysical : Unsigned_32 := 0;
    bootInfoBytes : Natural := Multiboot_Entry.Header_Bytes;
    bootUsesTags : Boolean := False;
    bootMapCount : Natural := 0;
    bootTagged : Multiboot2_Info.Snapshot;
    bootFramebuffer : Boot_Framebuffer.Result;
    -- Match the current userspace linear-buffer transport budget. This is an
    -- explicit boot backend limit, not a universal GPU/output size limit.
    Framebuffer_Budget : constant Boot_Framebuffer.Byte_Count := 16 * 1024 * 1024;
    pragma Compile_Time_Error
      (Framebuffer_Budget > Virtmem.FRAME_SIZE * 2 ** Config.MAX_BUDDY_ORDER,
       "Boot console budget exceeds the contiguous allocator block limit");

    function Has_Graphics return Boolean is
      (bootSnapshotReady and then bootFramebuffer.State = Boot_Framebuffer.Success)
      with SPARK_Mode => Off;

    function Tagged_Boot return Boolean is (bootUsesTags) with SPARK_Mode => Off;
    function Firmware_Root return Firmware_Tables.Root_Result is
      (if bootSnapshotReady and bootUsesTags then bootTagged.Root
       else (Status => Firmware_Tables.Truncated)) with SPARK_Mode => Off;

    function Firmware_Readable (Base, Length : Unsigned_64) return Boolean
      with SPARK_Mode => Off
    is
        use type Multiboot_Memory_Map.Region_Kind;
        Covered : Boolean := False;
        Last : Unsigned_64;
    begin
        if not bootSnapshotReady or else Base = 0 or else Length = 0 or else
          Base >= Virtmem.LINEAR_PHYSICAL_LIMIT or else
          Length > Virtmem.LINEAR_PHYSICAL_LIMIT - Base
        then return False; end if;
        Last := Base + (Length - 1);
        for Region of bootMapSnapshot (1 .. bootMapCount) loop
            if not Region.Empty and then Base <= Region.Last and then Region.First <= Last then
                if Region.Kind in Multiboot_Memory_Map.Usable | Multiboot_Memory_Map.Defective then
                    -- The legacy BIOS windows are retained, despite some
                    -- firmware maps describing parts of low RAM as usable.
                    if bootUsesTags or else Last >= 16#100000# then return False; end if;
                end if;
                if Region.First <= Base and then Last <= Region.Last then Covered := True; end if;
            end if;
        end loop;
        return Covered or else (not bootUsesTags and Last < 16#100000#);
    end Firmware_Readable;

    function Firmware_Reclaim_Pages (Base, Length : Unsigned_64) return Boolean
      with SPARK_Mode => Off
    is
    begin
        if not bootSnapshotReady or else Base = 0 or else Length = 0 or else
          Base mod Unsigned_64 (Virtmem.PAGE_SIZE) /= 0 or else
          Length mod Unsigned_64 (Virtmem.PAGE_SIZE) /= 0 or else
          Base >= Virtmem.LINEAR_PHYSICAL_LIMIT or else
          Length > Virtmem.LINEAR_PHYSICAL_LIMIT - Base
        then
            return False;
        end if;
        return Multiboot_Memory_Map.Reclaim.Covers
          (bootMapSnapshot (1 .. bootMapCount), Base, Base + (Length - 1));
    end Firmware_Reclaim_Pages;

    function Framebuffer return Boot_Framebuffer.Description
      with SPARK_Mode => Off
    is
    begin
        if not Has_Graphics then
            raise MemoryAreas.InvalidMemoryMap with "Boot framebuffer not published";
        end if;
        return bootFramebuffer.Value;
    end Framebuffer;
    type Captured_Module is record
        Descriptor : MBModule;
        Name : Boot_Modules.Module_Name;
        Name_First, Name_Limit : Boot_Modules.Address := 0;
    end record;
    capturedModules : array (Boot_Modules.Module_Index) of Captured_Module;

    function Boot_Module_Count return Boot_Modules.Module_Count
      with SPARK_Mode => Off
    is
    begin
        return (if bootSnapshotReady then Boot_Modules.Count (bootCatalog) else 0);
    end Boot_Module_Count;

    function Boot_Module (Index : Positive) return Boot_Modules.Image
      with SPARK_Mode => Off
    is
    begin
        if not bootSnapshotReady or else Index > Boot_Modules.Count (bootCatalog) then
            raise MemoryAreas.InvalidMemoryMap with "Boot module is not published";
        end if;
        return Boot_Modules.Get (bootCatalog, Index);
    end Boot_Module;

    procedure Read_Information
      (Magic, Physical : Unsigned_32; Info : out MultibootInfo)
      with SPARK_Mode => Off -- admitted address overlay and record representation
    is
        package Entry_Check renames Multiboot_Entry;
        use type Entry_Check.Status;
        Result : Entry_Check.Status;
        Snapshot : Entry_Check.Snapshot_Header;
        pragma Compile_Time_Error
          (MultibootInfo'Size /= Entry_Check.Snapshot_Bytes * 8,
           "Boot header snapshot must match the Multiboot record layout");
    begin
        if Magic = Multiboot2_Info.Loader_Magic then
            if Physical = 0 or else Physical mod 8 /= 0 or else
              Unsigned_64 (Physical) > Multiboot_Entry.Bootstrap_Limit - 8
            then
                raise MemoryAreas.InvalidMemoryMap with "Invalid Multiboot2 prefix address";
            end if;
            if Overlaps_Kernel (Integer_Address (Physical), 8) then
                raise MemoryAreas.InvalidMemoryMap with "Multiboot2 prefix overlaps kernel";
            end if;
            declare
                Prefix : Multiboot2_Info.Bytes (0 .. 7) with Import,
                  Address => To_Address (Integer_Address (Physical));
            begin
                bootInfoBytes := Multiboot2_Info.Header_Extent (Prefix);
            end;
            if bootInfoBytes = 0 or else Unsigned_64 (bootInfoBytes) >
              Multiboot_Entry.Bootstrap_Limit - Unsigned_64 (Physical) or else
              Overlaps_Kernel (Integer_Address (Physical), Integer_Address (bootInfoBytes))
            then
                raise MemoryAreas.InvalidMemoryMap with "Invalid Multiboot2 metadata extent";
            end if;
            declare
                Raw : Multiboot2_Info.Bytes (0 .. bootInfoBytes - 1) with Import,
                  Address => To_Address (Integer_Address (Physical));
                State : Multiboot2_Info.Status;
                use type Multiboot2_Info.Status;
                Destination : Entry_Check.Snapshot_Header with Import, Address => Info'Address;
            begin
                Multiboot2_Info.Parse (Raw, Unsigned_64 (Virtmem.PhysAddress'Last),
                  bootTagged, bootMapSnapshot, bootMapCount, State);
                if State /= Multiboot2_Info.Success then
                    print ("Multiboot2 admission: "); println (Multiboot2_Info.Status'Image (State));
                    raise MemoryAreas.InvalidMemoryMap with "Invalid Multiboot2 metadata";
                end if;
                Destination := [others => 0];
            end;
            bootUsesTags := True;
            Info.flags.hasMemoryMap := True;
            Info.flags.hasFramebuffer := True;
            Info.framebuffer_addr := Integer_Address (bootTagged.Frame.Base);
            Info.framebuffer_pitch := bootTagged.Frame.Pitch;
            Info.framebuffer_width := bootTagged.Frame.Width;
            Info.framebuffer_height := bootTagged.Frame.Height;
            Info.framebuffer_bpp := bootTagged.Frame.Depth;
            Info.framebuffer_type := bootTagged.Frame.Kind;
            Info.framebuffer_red_field_position := bootTagged.Frame.Red_Position;
            Info.framebuffer_red_mask_size := bootTagged.Frame.Red_Size;
            Info.framebuffer_green_field_position := bootTagged.Frame.Green_Position;
            Info.framebuffer_green_mask_size := bootTagged.Frame.Green_Size;
            Info.framebuffer_blue_field_position := bootTagged.Frame.Blue_Position;
            Info.framebuffer_blue_mask_size := bootTagged.Frame.Blue_Size;
            println ("Multiboot2: owned boot metadata admitted");
        else
        Entry_Check.Admit_Address
          (Magic, Unsigned_64 (Physical), Entry_Check.Bootstrap_Limit, Result);
        if Result /= Entry_Check.Success then
            raise MemoryAreas.InvalidMemoryMap with "Invalid boot information address/magic";
        end if;
        if Overlaps_Kernel (Integer_Address (Physical), Entry_Check.Header_Bytes) then
            raise MemoryAreas.InvalidMemoryMap with "Boot header overlaps kernel storage";
        end if;
        declare
            Raw : Entry_Check.Header with Import,
              Address => To_Address (Integer_Address (Physical));
        begin
            Entry_Check.Snapshot (Raw, Snapshot, Result);
        end;
        if Result /= Entry_Check.Success then
            print ("Boot information: "); println (Entry_Check.Status'Image (Result));
            raise MemoryAreas.InvalidMemoryMap with "Unsupported boot information";
        end if;
        declare
            -- The native record's Object_Size includes alignment padding
            -- beyond the 116-byte wire format. Copy bytes INTO that larger
            -- object, never overlay a native record on the smaller buffer.
            Destination : Entry_Check.Snapshot_Header with Import, Address => Info'Address;
        begin
            Destination := Snapshot;
        end;
        end if;
        bootInfoPhysical := Physical;
        -- Admit geometry before even selecting the early text/graphics adapter.
        -- The direct-map region is narrower than PhysAddress on this platform.
        bootFramebuffer := Boot_Framebuffer.Decode
          ((Base => Unsigned_64 (Info.framebuffer_addr),
            Width => Info.framebuffer_width, Height => Info.framebuffer_height,
            Pitch => Info.framebuffer_pitch, Kind => Info.framebuffer_type,
            Depth => Info.framebuffer_bpp,
            Red_Position => Info.framebuffer_red_field_position,
            Red_Size => Info.framebuffer_red_mask_size,
            Green_Position => Info.framebuffer_green_field_position,
            Green_Size => Info.framebuffer_green_mask_size,
            Blue_Position => Info.framebuffer_blue_field_position,
            Blue_Size => Info.framebuffer_blue_mask_size),
           Boot_Framebuffer.Address (Unsigned_64'Min
             (Virtmem.LINEAR_PHYSICAL_LIMIT, CPUID.Physical_Address_Limit)),
           Framebuffer_Budget);
        case bootFramebuffer.State is
            when Boot_Framebuffer.Success =>
                -- Retain the bootstrap diagnostic mode admission floor.
                -- The fixed panel clips within it; native GPUs are separate.
                if bootFramebuffer.Value.Width < Boot_Font.Width + 1 or else
                  bootFramebuffer.Value.Height < 2 * Boot_Font.Height
                then
                    raise MemoryAreas.InvalidMemoryMap with "Boot framebuffer too small for diagnostics";
                end if;
            when Boot_Framebuffer.Text_Mode => null;
            when Boot_Framebuffer.Unsupported_Format =>
                raise MemoryAreas.InvalidMemoryMap with "Unsupported boot framebuffer format";
            when Boot_Framebuffer.Invalid_Geometry =>
                raise MemoryAreas.InvalidMemoryMap with "Invalid boot framebuffer geometry";
            when Boot_Framebuffer.Budget_Exceeded =>
                raise MemoryAreas.InvalidMemoryMap with "Boot framebuffer exceeds backend budget";
            when Boot_Framebuffer.Invalid_Address =>
                raise MemoryAreas.InvalidMemoryMap with "Invalid boot framebuffer mapping extent";
        end case;
    end Read_Information;


    procedure getMemoryAreas
      (mbinfo : MultibootInfo; Areas : out Boot_Memory_Map; Last : out Natural)
      with SPARK_Mode => Off -- validated early-boot address overlays
    is
        package Decoder renames Multiboot_Memory_Map;
        use type Decoder.Status;
        use type Decoder.Region_Kind;
        Kinds : constant array (Decoder.Region_Kind) of MemoryAreas.MemoryAreaType :=
          [Decoder.Usable => MemoryAreas.USABLE,
           Decoder.Reserved => MemoryAreas.RESERVED,
           Decoder.ACPI_Reclaim => MemoryAreas.ACPI,
           Decoder.ACPI_NVS => MemoryAreas.HIBERNATE,
           Decoder.Defective => MemoryAreas.BAD];
        Count : Natural;
        Result : Decoder.Status;
        moduleCount : Boot_Modules.Module_Count := 0;
        bootEnd : Integer_Address :=
          Virtmem.V2P (To_Address (Virtmem.STACK_TOP));

        procedure Require_Boot_Window (Base, Length : Integer_Address) is
        begin
            if Base = 0 or else Length = 0 or else
              Base >= Virtmem.BOOTSTRAP_PHYSICAL_LIMIT or else
              Length > Virtmem.BOOTSTRAP_PHYSICAL_LIMIT - Base
            then
                raise MemoryAreas.InvalidMemoryMap with
                  "Loader buffer outside bootstrap mapping";
            end if;
            if Overlaps_Kernel (Base, Length) then
                raise MemoryAreas.InvalidMemoryMap with "Boot source overlaps kernel storage";
            end if;
        end Require_Boot_Window;

        procedure Require_Source_RAM (Base, Length : Integer_Address) is
        begin
            Require_Boot_Window (Base, Length);
            if not Boot_Modules.In_RAM (bootMapSnapshot (1 .. Count),
              Boot_Modules.Address (Base), Boot_Modules.Address (Base + Length))
            then
                raise MemoryAreas.InvalidMemoryMap with "Boot metadata is not independent RAM";
            end if;
        end Require_Source_RAM;

        procedure Reject_Metadata_Overlap
          (Item : Boot_Modules.Image; Base, Length : Integer_Address) is
        begin
            if Integer_Address (Item.First) < Base + Length and then
              Base < Integer_Address (Item.Page_Limit)
            then
                raise MemoryAreas.InvalidMemoryMap with "Boot payload overlaps metadata";
            end if;
        end Reject_Metadata_Overlap;
    begin
        Last := 0;
        Areas := [others => MemoryAreas.Empty_Area];
        if Boot_Modules.Sealed (bootCatalog) then
            raise MemoryAreas.InvalidMemoryMap with "Boot module snapshot already sealed";
        end if;
        if not mbinfo.flags.hasMemoryMap then
            raise MemoryAreas.InvalidMemoryMap with "Missing Multiboot memory map";
        end if;
        if bootUsesTags then
            Count := bootMapCount;
            Require_Source_RAM (Integer_Address (bootInfoPhysical), Integer_Address (bootInfoBytes));
            bootEnd := Integer_Address'Max (bootEnd,
              Integer_Address (bootInfoPhysical) + Integer_Address (bootInfoBytes));
            moduleCount := bootTagged.Module_Count;
            for I in 1 .. moduleCount loop
                capturedModules (I).Descriptor :=
                  (mod_start => bootTagged.Modules (I).First,
                   mod_end => bootTagged.Modules (I).Limit, others => 0);
                capturedModules (I).Name := bootTagged.Modules (I).Name;
            end loop;
        else
        Require_Boot_Window (Integer_Address (mbinfo.mmap_addr),
                             Integer_Address (mbinfo.mmap_length));
        declare
            -- The extent is checked BEFORE constructing the overlay. Its
            -- bytes are only read while the bootstrap mapping is active.
            Data : Decoder.Bytes (0 .. Natural (mbinfo.mmap_length) - 1)
              with Import, Address => To_Address (Integer_Address (mbinfo.mmap_addr));
        begin
            Decoder.Parse (Data, Unsigned_64 (Virtmem.PhysAddress'Last),
                           bootMapSnapshot, Count, Result);
        end;
        if Result /= Decoder.Success then
            print ("Boot map decoder: "); println (Decoder.Status'Image (Result));
            raise MemoryAreas.InvalidMemoryMap with "Invalid Multiboot memory map";
        end if;

        -- Freeze descriptors and bounded NUL-terminated names before either
        -- allocator starts. No later consumer reopens these loader buffers.
        Require_Source_RAM (Integer_Address (mbinfo.mmap_addr),
                            Integer_Address (mbinfo.mmap_length));
        Require_Source_RAM (Integer_Address (bootInfoPhysical), Multiboot_Entry.Header_Bytes);
        bootEnd := Integer_Address'Max (bootEnd,
          Integer_Address (mbinfo.mmap_addr) + Integer_Address (mbinfo.mmap_length));
        bootEnd := Integer_Address'Max (bootEnd,
          Integer_Address (bootInfoPhysical) + Multiboot_Entry.Header_Bytes);
        if mbinfo.flags.hasModules and then mbinfo.mods_count /= 0 then
            if mbinfo.mods_count > Boot_Modules.Maximum_Modules then
                raise MemoryAreas.InvalidMemoryMap with "Boot module catalog capacity exceeded";
            end if;
            moduleCount := Boot_Modules.Module_Count (mbinfo.mods_count);
            Require_Source_RAM (Integer_Address (mbinfo.mods_addr),
                                Integer_Address (moduleCount) * Module_Bytes);
            bootEnd := Integer_Address'Max (bootEnd,
              Integer_Address (mbinfo.mods_addr) + Integer_Address (moduleCount) * Module_Bytes);
            for I in 1 .. moduleCount loop
                declare
                    Raw : MBModule with Import, Address => Virtmem.P2Va
                      (Integer_Address (mbinfo.mods_addr) + Integer_Address (I - 1) * Module_Bytes);
                begin
                    capturedModules (I).Descriptor := Raw;
                end;
            end loop;
            for I in 1 .. moduleCount loop
                declare
                    Base : constant Integer_Address :=
                      Integer_Address (capturedModules (I).Descriptor.mod_string);
                    terminated : Boolean := False;
                begin
                    for J in 1 .. Boot_Modules.Maximum_Name + 1 loop
                        Require_Source_RAM (Base + Integer_Address (J - 1), 1);
                        declare
                            Byte : Unsigned_8 with Import, Address =>
                              Virtmem.P2Va (Base + Integer_Address (J - 1));
                            Value : constant Unsigned_8 := Byte;
                        begin
                            if Value = 0 then
                                capturedModules (I).Name_First := Boot_Modules.Address (Base);
                                capturedModules (I).Name_Limit := Boot_Modules.Address (Base + Integer_Address (J));
                                bootEnd := Integer_Address'Max (bootEnd, Base + Integer_Address (J));
                                terminated := True;
                                exit;
                            elsif J > Boot_Modules.Maximum_Name then
                                exit;
                            end if;
                            capturedModules (I).Name.Text (J) := Character'Val (Value);
                            capturedModules (I).Name.Length := J;
                        end;
                    end loop;
                    if not terminated then
                        raise MemoryAreas.InvalidMemoryMap with "Unterminated boot module name";
                    end if;
                end;
            end loop;
        end if;
        end if; -- loader-specific capture; lifetime admission below is shared
        bootMapCount := Count;
        for I in 1 .. moduleCount loop
            declare
                Status : Boot_Modules.Status;
                use type Boot_Modules.Status;
            begin
                Boot_Modules.Append (bootCatalog, bootMapSnapshot (1 .. Count),
                  Unsigned_64 (capturedModules (I).Descriptor.mod_start),
                  Unsigned_64 (capturedModules (I).Descriptor.mod_end),
                  Boot_Modules.Address (Virtmem.V2P (To_Address (Virtmem.STACK_TOP))),
                  capturedModules (I).Name, Status);
                if Status /= Boot_Modules.Success then
                    print ("Boot module admission: ");
                    -- The minimal kernel runtime renders Enum'Image as a
                    -- number; keep fatal admission diagnostics explicit.
                    case Status is
                        when Boot_Modules.Duplicate_Name => println ("duplicate module name");
                        when Boot_Modules.Overlapping_Payload => println ("overlapping payload pages");
                        when Boot_Modules.Invalid_Range => println ("invalid payload range/alignment");
                        when Boot_Modules.Invalid_Name => println ("empty module name");
                        when Boot_Modules.Not_RAM => println ("payload pages are not usable RAM");
                        when Boot_Modules.Capacity_Exceeded => println ("catalog capacity exceeded");
                        when Boot_Modules.Already_Sealed => println ("catalog already sealed");
                        when Boot_Modules.Success => null;
                    end case;
                    raise MemoryAreas.InvalidMemoryMap with "Invalid boot module catalog";
                end if;
            end;
        end loop;
        Boot_Modules.Seal (bootCatalog);
        bootEnd := Integer_Address'Max (bootEnd,
          Integer_Address (Boot_Modules.Reserved_End (bootCatalog)));
        for I in 1 .. moduleCount loop
            declare
                Item : constant Boot_Modules.Image := Boot_Modules.Get (bootCatalog, I);
            begin
                Reject_Metadata_Overlap (Item, Integer_Address (bootInfoPhysical), Integer_Address (bootInfoBytes));
                if not bootUsesTags then
                Reject_Metadata_Overlap (Item, Integer_Address (mbinfo.mmap_addr), Integer_Address (mbinfo.mmap_length));
                Reject_Metadata_Overlap (Item, Integer_Address (mbinfo.mods_addr), Integer_Address (moduleCount) * Module_Bytes);
                for J in 1 .. moduleCount loop
                    Reject_Metadata_Overlap (Item, Integer_Address (capturedModules (J).Name_First),
                      Integer_Address (capturedModules (J).Name_Limit - capturedModules (J).Name_First));
                end loop;
                end if;
            end;
        end loop;
        bootEnd := (bootEnd + 4095) and not Integer_Address (4095);
        if mbinfo.flags.hasFramebuffer and then mbinfo.framebuffer_type = 2 then
            print ("Boot reserved arena ends at "); println (bootEnd);
        end if;

        -- Consume the decoded snapshot; never rewalk the variable-length input.
        for I in 1 .. Count loop
            if not bootMapSnapshot (I).Empty then
                Areas (I) := (kind => Kinds (bootMapSnapshot (I).Kind),
                              startAddr => Integer_Address (bootMapSnapshot (I).First),
                              endAddr => Integer_Address (bootMapSnapshot (I).Last));
                if bootMapSnapshot (I).Kind = Decoder.Usable then
                    if Areas (I).endAddr < bootEnd then
                        Areas (I).kind := MemoryAreas.RESERVED;
                    elsif Areas (I).startAddr < bootEnd then
                        Areas (I).startAddr := bootEnd;
                    end if;
                end if;
            end if;
        end loop;
        Areas (Count + 1) :=
          (kind => MemoryAreas.RESERVED, startAddr => 16#100000#, endAddr => bootEnd - 1);
        if bootFramebuffer.State = Boot_Framebuffer.Success then
            declare
                Item : constant Boot_Framebuffer.Description := bootFramebuffer.Value;
                use type Boot_Framebuffer.Address;
            begin
                -- Protect entire mapped pages, including leading/trailing slack.
                -- The retained prefix includes kernel, stack, all loader metadata
                -- and payloads; firmware may use RAM elsewhere for scanout.
                if Item.Map_First < Boot_Framebuffer.Address (bootEnd) then
                    raise MemoryAreas.InvalidMemoryMap with "Boot framebuffer overlaps retained boot memory";
                end if;
                for Region of bootMapSnapshot (1 .. Count) loop
                    if not Region.Empty and then
                      Region.Kind in Decoder.ACPI_Reclaim | Decoder.ACPI_NVS | Decoder.Defective and then
                      Boot_Framebuffer.Overlaps (Item,
                        Boot_Framebuffer.Address (Region.First),
                        Boot_Framebuffer.Address (Region.Last) + 1)
                    then
                        raise MemoryAreas.InvalidMemoryMap with "Boot framebuffer overlaps protected firmware memory";
                    end if;
                end loop;
                Areas (Count + 2) :=
                  (kind => MemoryAreas.VIDEO,
                   startAddr => Integer_Address (Item.Map_First),
                   endAddr => Integer_Address (Item.Map_Limit - 1));
            end;
        end if;
        -- The only userspace exposure is page-granular and read-only. Scrub
        -- final-page slack after ALL payload/metadata conflicts are rejected.
        for I in 1 .. moduleCount loop
            declare
                Item : constant Boot_Modules.Image := Boot_Modules.Get (bootCatalog, I);
            begin
                if Item.Limit < Item.Page_Limit then
                    declare
                        Payload : Decoder.Bytes (0 .. Natural (Item.Page_Limit - Item.First) - 1)
                          with Import, Address => Virtmem.P2Va (Integer_Address (Item.First));
                    begin
                        Boot_Modules.Clear_Padding (Payload, Natural (Item.Limit - Item.First));
                    end;
                end if;
            end;
        end loop;
        bootSnapshotReady := True;
        Last := Count + 2;
    end getMemoryAreas;
end Multiboot;
