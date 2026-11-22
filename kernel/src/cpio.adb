-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Kernel-side CPIO newc archive parser.
--
-- CPIO newc header (110 bytes ASCII hex):
--   "070701" magic (6 bytes)
--   c_ino(8) c_mode(8) c_uid(8) c_gid(8) c_nlink(8) c_mtime(8)
--   c_filesize(8) c_devmajor(8) c_devminor(8) c_rdevmajor(8)
--   c_rdevminor(8) c_namesize(8) c_check(8)
-- Filename follows header, padded to 4-byte boundary.
-- File data follows filename, padded to 4-byte boundary.
-- Archive ends with entry named "TRAILER!!!".
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with TextIO; use TextIO;

package body Cpio is

    HEADER_SIZE : constant := 110;
    MAGIC_LEN   : constant := 6;

    ---------------------------------------------------------------------------
    -- parseHex8
    -- Parse 8 ASCII hex characters at addr into a 32-bit value.
    ---------------------------------------------------------------------------
    function parseHex8 (addr : System.Address) return Unsigned_32 with
        SPARK_Mode => On
    is
        chars  : String (1 .. 8) with Import, Address => addr;
        result : Unsigned_32 := 0;
        nibble : Unsigned_32;
    begin
        for i in 1 .. 8 loop
            case chars (i) is
                when '0' .. '9' =>
                    nibble := Character'Pos (chars (i)) - Character'Pos ('0');
                when 'a' .. 'f' =>
                    nibble := Character'Pos (chars (i)) - Character'Pos ('a') + 10;
                when 'A' .. 'F' =>
                    nibble := Character'Pos (chars (i)) - Character'Pos ('A') + 10;
                when others =>
                    return 0;
            end case;
            result := Shift_Left (result, 4) or nibble;
        end loop;
        return result;
    end parseHex8;

    ---------------------------------------------------------------------------
    -- alignUp4
    ---------------------------------------------------------------------------
    function alignUp4 (v : Storage_Count) return Storage_Count with
        SPARK_Mode => On
    is
    begin
        return ((v + 3) / 4) * 4;
    end alignUp4;

    ---------------------------------------------------------------------------
    -- checkMagic
    ---------------------------------------------------------------------------
    function checkMagic (addr : System.Address) return Boolean with
        SPARK_Mode => On
    is
        magic : String (1 .. MAGIC_LEN) with Import, Address => addr;
    begin
        return magic (1) = '0' and then
               magic (2) = '7' and then
               magic (3) = '0' and then
               magic (4) = '7' and then
               magic (5) = '0' and then
               magic (6) = '1';
    end checkMagic;

    ---------------------------------------------------------------------------
    -- isTrailer
    ---------------------------------------------------------------------------
    function isTrailer
      (addr : System.Address; len : Natural) return Boolean with
        SPARK_Mode => On
    is
        name : String (1 .. len) with Import, Address => addr;
    begin
        if len /= 10 then
            return False;
        end if;
        return name (1)  = 'T' and then
               name (2)  = 'R' and then
               name (3)  = 'A' and then
               name (4)  = 'I' and then
               name (5)  = 'L' and then
               name (6)  = 'E' and then
               name (7)  = 'R' and then
               name (8)  = '!' and then
               name (9)  = '!' and then
               name (10) = '!';
    end isTrailer;

    ---------------------------------------------------------------------------
    -- init
    ---------------------------------------------------------------------------
    procedure init
      (ar   : out Archive;
       base : System.Address;
       size : Storage_Count;
       ok   : out Boolean)
    is
        pos      : Storage_Count := 0;
        nameSize : Unsigned_32;
        fileSize : Unsigned_32;
        nameOff  : Storage_Count;
        dataOff  : Storage_Count;
    begin
        ar.base  := base;
        ar.size  := size;
        ar.count := 0;

        loop
            -- Need at least a full header
            if pos + HEADER_SIZE > size then
                ok := True;
                return;
            end if;

            -- Check magic
            if not checkMagic (base + Storage_Offset (pos)) then
                println ("Cpio: bad magic in initrd.");
                ok := False;
                return;
            end if;

            -- Parse c_namesize at offset 94 and c_filesize at offset 54
            nameSize := parseHex8 (base + Storage_Offset (pos + 94));
            fileSize := parseHex8 (base + Storage_Offset (pos + 54));

            -- Filename starts right after the header
            nameOff := pos + HEADER_SIZE;

            -- Check for TRAILER!!!
            if isTrailer (base + Storage_Offset (nameOff),
                          Natural (nameSize) - 1)
            then
                ok := True;
                return;
            end if;

            -- Data starts after header + name, aligned to 4 bytes
            dataOff := alignUp4 (nameOff + Storage_Count (nameSize));

            -- Only index regular files (skip directory entries)
            if nameSize > 1 and fileSize > 0 then
                if ar.count < MAX_FILES then
                    ar.files (ar.count) :=
                      (dataAddr => base + Storage_Offset (dataOff),
                       dataSize => Storage_Count (fileSize),
                       nameAddr => base + Storage_Offset (nameOff),
                       nameLen  => Natural (nameSize) - 1);
                    ar.count := ar.count + 1;
                end if;
            end if;

            -- Advance past data, aligned to 4 bytes
            pos := alignUp4 (dataOff + Storage_Count (fileSize));
        end loop;
    end init;

    ---------------------------------------------------------------------------
    -- findFile
    ---------------------------------------------------------------------------
    function findFile (ar : Archive; name : String) return Natural is
    begin
        for i in 0 .. ar.count - 1 loop
            if ar.files (i).nameLen = name'Length then
                declare
                    arName : String (1 .. ar.files (i).nameLen)
                      with Import,
                           Address => ar.files (i).nameAddr;
                    match : Boolean := True;
                begin
                    for j in 1 .. name'Length loop
                        if arName (j) /= name (name'First + j - 1) then
                            match := False;
                            exit;
                        end if;
                    end loop;

                    if match then
                        return i;
                    end if;
                end;
            end if;
        end loop;

        return ar.count;  -- not found
    end findFile;

end Cpio;
