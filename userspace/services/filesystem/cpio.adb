------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CPIO newc archive parser.
--
--  CPIO newc header (110 bytes ASCII hex):
--    "070701" magic (6 bytes)
--    c_ino(8) c_mode(8) c_uid(8) c_gid(8) c_nlink(8) c_mtime(8)
--    c_filesize(8) c_devmajor(8) c_devminor(8) c_rdevmajor(8)
--    c_rdevminor(8) c_namesize(8) c_check(8)
--  Filename follows header, padded to 4-byte boundary.
--  File data follows filename, padded to 4-byte boundary.
--  Archive ends with entry named "TRAILER!!!".
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body Cpio is

   HEADER_SIZE : constant := 110;
   MAGIC_LEN   : constant := 6;

   ---------------------------------------------------------------------------
   --  parseHex8
   --  Parse 8 ASCII hex characters at addr into a 32-bit value.
   ---------------------------------------------------------------------------
   function parseHex8 (addr : System.Address) return Unsigned_32 is
      chars : String (1 .. 8) with Import, Address => addr;
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
   --  alignUp4
   --  Round up to next 4-byte boundary.
   ---------------------------------------------------------------------------
   function alignUp4 (v : Unsigned_64) return Unsigned_64 is
   begin
      return (v + 3) and not Unsigned_64'(3);
   end alignUp4;

   ---------------------------------------------------------------------------
   --  checkMagic
   --  Verify "070701" magic at the given address.
   ---------------------------------------------------------------------------
   function checkMagic (addr : System.Address) return Boolean is
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
   --  isTrailer
   --  Check if filename at addr with given length is "TRAILER!!!".
   ---------------------------------------------------------------------------
   function isTrailer
     (addr : System.Address; len : Natural) return Boolean
   is
      name : String (1 .. len) with Import, Address => addr;
   begin
      if len /= 10 then
         return False;
      end if;
      return name (1) = 'T' and then
             name (2) = 'R' and then
             name (3) = 'A' and then
             name (4) = 'I' and then
             name (5) = 'L' and then
             name (6) = 'E' and then
             name (7) = 'R' and then
             name (8) = '!' and then
             name (9) = '!' and then
             name (10) = '!';
   end isTrailer;

   ---------------------------------------------------------------------------
   --  init
   ---------------------------------------------------------------------------
   procedure init
     (ar   : out Archive;
      base : System.Address;
      size : Unsigned_64;
      ok   : out Boolean)
   is
      pos      : Unsigned_64 := 0;
      nameSize : Unsigned_32;
      fileSize : Unsigned_32;
      nameOff  : Unsigned_64;
      dataOff  : Unsigned_64;
   begin
      ar.base  := base;
      ar.size  := size;
      ar.count := 0;

      loop
         --  Need at least a full header
         if pos + HEADER_SIZE > size then
            ok := True;
            return;
         end if;

         --  Check magic
         if not checkMagic (base + Storage_Offset (pos)) then
            debugPrint ("Cpio: bad magic at offset." & ASCII.LF);
            ok := False;
            return;
         end if;

         --  Parse c_namesize at offset 94 and c_filesize at offset 54
         nameSize := parseHex8 (base + Storage_Offset (pos + 94));
         fileSize := parseHex8 (base + Storage_Offset (pos + 54));

         --  Filename starts right after the header
         nameOff := pos + HEADER_SIZE;

         --  Check for TRAILER!!!
         if isTrailer (base + Storage_Offset (nameOff),
                       Natural (nameSize) - 1)
         then
            ok := True;
            return;
         end if;

         --  Data starts after header + name, aligned to 4 bytes
         dataOff := alignUp4 (nameOff + Unsigned_64 (nameSize));

         --  Only index regular files (skip "." directory entries)
         --  nameSize includes the trailing NUL, so actual name length
         --  is nameSize - 1. Skip entries with nameSize <= 1.
         if nameSize > 1 and fileSize > 0 then
            if ar.count < MAX_FILES then
               ar.files (ar.count) :=
                 (nameOff  => nameOff,
                  nameLen  => Natural (nameSize) - 1,
                  dataOff  => dataOff,
                  dataSize => Unsigned_64 (fileSize));
               ar.count := ar.count + 1;
            end if;
         end if;

         --  Advance past data, aligned to 4 bytes
         pos := alignUp4 (dataOff + Unsigned_64 (fileSize));
      end loop;
   end init;

   ---------------------------------------------------------------------------
   --  findFile
   ---------------------------------------------------------------------------
   function findFile (ar : Archive; name : String) return Natural is
   begin
      for i in 0 .. ar.count - 1 loop
         if ar.files (i).nameLen = name'Length then
            declare
               arName : String (1 .. ar.files (i).nameLen)
                 with Import,
                      Address => ar.base +
                        Storage_Offset (ar.files (i).nameOff);
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

      return ar.count;  --  not found
   end findFile;

   ---------------------------------------------------------------------------
   --  readData
   ---------------------------------------------------------------------------
   function readData
     (ar      : Archive;
      fileIdx : Natural;
      offset  : Unsigned_64;
      buf     : System.Address;
      count   : Unsigned_64) return Unsigned_64
   is
      fi        : FileInfo renames ar.files (fileIdx);
      available : Unsigned_64;
      toRead    : Unsigned_64;
   begin
      if offset >= fi.dataSize then
         return 0;
      end if;

      available := fi.dataSize - offset;
      toRead := count;
      if toRead > available then
         toRead := available;
      end if;

      declare
         src : String (1 .. Natural (toRead))
           with Import,
                Address => ar.base +
                  Storage_Offset (fi.dataOff + offset);
         dst : String (1 .. Natural (toRead))
           with Import, Address => buf;
      begin
         dst := src;
      end;

      return toRead;
   end readData;

   ---------------------------------------------------------------------------
   --  listFiles
   ---------------------------------------------------------------------------
   function listFiles
     (ar       : Archive;
      dest     : System.Address;
      destSize : Unsigned_64) return Unsigned_64
   is
      written : Unsigned_64 := 0;
      outBuf  : String (1 .. Natural (destSize))
        with Import, Address => dest;
   begin
      for i in 0 .. ar.count - 1 loop
         declare
            nameLen : constant Natural := ar.files (i).nameLen;
            arName  : String (1 .. nameLen)
              with Import,
                   Address => ar.base +
                     Storage_Offset (ar.files (i).nameOff);
         begin
            --  Check room for name + newline
            if written + Unsigned_64 (nameLen) + 1 > destSize then
               return written;
            end if;

            for j in 1 .. nameLen loop
               outBuf (Natural (written) + j) := arName (j);
            end loop;
            written := written + Unsigned_64 (nameLen);
            outBuf (Natural (written) + 1) := ASCII.LF;
            written := written + 1;
         end;
      end loop;

      return written;
   end listFiles;

end Cpio;
