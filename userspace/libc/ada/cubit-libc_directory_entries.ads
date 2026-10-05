------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  getdents64 over the filesystem service's Directory.Page.V1
--  (CuBit.Filesystems; docs/c-removal.md): one page entry as one Linux
--  struct linux_dirent64 in the caller's buffer.
--
--  @description
--  The page comes from the filesystem service and is read byte by byte, so
--  nothing in it is trusted: a page is used only with the layout this libc
--  knows, and names are cut to 255 bytes. Proved (tests/libc-ada): every
--  read stays in the page, every write in the caller's buffer, and records
--  are 8-byte multiples that never pass the buffer's end.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_Directory_Entries with Pure, SPARK_Mode is

   --  Directory.Page.V1 (CuBit.Filesystems; tests/libc-ada checks these).
   Page_Bytes        : constant := 4_096;
   Page_Header_Bytes : constant := 32;
   Entry_Bytes       : constant := 280;
   Maximum_Entries   : constant := 14;
   Name_Offset       : constant := 24;   --  in an entry
   Name_Length_Offset : constant := 16;
   Kind_Offset       : constant := 18;
   Header_Bytes_Offset : constant := 2;
   Entry_Bytes_Offset  : constant := 4;
   Entry_Count_Offset  : constant := 6;
   Flags_Offset        : constant := 8;
   Page_End            : constant := 1;
   Kind_File      : constant := 1;
   Kind_Directory : constant := 2;
   Kind_Symlink   : constant := 3;
   --  A Linux name is at most 255 bytes (NAME_MAX).
   Maximum_Name_Bytes : constant := 255;

   --  struct linux_dirent64: d_ino, d_off, d_reclen, d_type, d_name.
   Dirent_Name_Offset : constant := 19;
   Record_Alignment : constant := 8;
   DT_UNKNOWN : constant := 0;
   DT_DIR     : constant := 4;
   DT_REG     : constant := 8;
   DT_LNK     : constant := 10;

   subtype Page_Index is Natural range 0 .. Page_Bytes - 1;
   type Page is array (Page_Index) of Unsigned_8;
   subtype Entry_Index is Natural range 0 .. Maximum_Entries - 1;
   subtype Entry_Count is Natural range 0 .. Maximum_Entries;

   type Bytes is array (Natural range <>) of Unsigned_8;
   --  The largest buffer getdents fills in one call.
   Maximum_Buffer_Bytes : constant := 2 ** 30;

   function Entry_Start (Index : Entry_Index) return Page_Index is
     (Page_Header_Bytes + Index * Entry_Bytes);

   function U16 (P : Page; At_Byte : Natural) return Unsigned_16 is
     (Unsigned_16 (P (At_Byte)) or Shift_Left (Unsigned_16 (P (At_Byte + 1)), 8))
   with Pre => At_Byte < Page_Bytes - 1;

   --  The page's entry count if its layout is the one known, else none.
   procedure Header
     (P : Page; Valid : out Boolean; Count : out Entry_Count; Ended : out Boolean);

   --  Entry Index's name length, cut to Maximum_Name_Bytes.
   function Name_Length (P : Page; Index : Entry_Index) return Natural is
     (Natural'Min (Natural (U16 (P, Entry_Start (Index) + Name_Length_Offset)),
                   Maximum_Name_Bytes));

   --  The record entry Index takes in a buffer: its name and a NUL after
   --  the fixed fields, rounded up to 8 bytes.
   function Record_Bytes (P : Page; Index : Entry_Index) return Positive
   with Post => Record_Bytes'Result mod Record_Alignment = 0
                and then Record_Bytes'Result >= Dirent_Name_Offset + Name_Length (P, Index) + 1
                and then Record_Bytes'Result <=
                  Dirent_Name_Offset + Maximum_Name_Bytes + 1 + Record_Alignment;

   --  Write entry Index at Buffer (Used ..) if it fits in Buffer'Last.
   procedure Encode
     (P : Page; Index : Entry_Index; Buffer : in out Bytes; Used : in out Natural;
      Fits : out Boolean)
   with Pre => Buffer'First = 0
               and then Buffer'Last in -1 .. Maximum_Buffer_Bytes - 1
               and then Used <= Buffer'Last + 1,
        Post => Used <= Buffer'Last + 1
                and then (if Fits then Used = Used'Old + Record_Bytes (P, Index)
                          else Used = Used'Old);

end CuBit.Libc_Directory_Entries;
