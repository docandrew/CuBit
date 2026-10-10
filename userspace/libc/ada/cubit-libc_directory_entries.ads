------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  getdents64 over the filesystem service's Directory.Page.V2
--  (CuBit.Directory_Pages; docs/c-removal.md): one page entry as one Linux
--  struct linux_dirent64 in the caller's buffer.
--
--  @description
--  The page comes from the filesystem service, so nothing in it is
--  trusted: a page is used only after CuBit.Directory_Pages.Check accepted
--  the private copy, and each entry is read through its checked Get.
--  Proved (tests/libc-ada): every write stays in the caller's buffer, and
--  records are 8-byte multiples that never pass the buffer's end.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Directory_Pages;

package CuBit.Libc_Directory_Entries with Pure, SPARK_Mode is

   package DP renames CuBit.Directory_Pages;

   --  struct linux_dirent64: d_ino, d_off, d_reclen, d_type, d_name.
   Dirent_Name_Offset : constant := 19;
   Record_Alignment : constant := 8;
   DT_UNKNOWN : constant := 0;
   DT_DIR     : constant := 4;
   DT_REG     : constant := 8;
   DT_LNK     : constant := 10;
   Largest_Dirent : constant :=
     (Dirent_Name_Offset + DP.Maximum_Name_Bytes + 1 + Record_Alignment - 1)
     / Record_Alignment * Record_Alignment;

   type Bytes is array (Natural range <>) of Unsigned_8;
   --  The largest buffer getdents fills in one call.
   Maximum_Buffer_Bytes : constant := 2 ** 30;

   --  The record a name of Length bytes takes in a buffer: the fixed
   --  fields, the name and a NUL, rounded up to 8 bytes.
   function Record_Bytes (Length : DP.Name_Length) return Positive is
     ((Dirent_Name_Offset + Length + 1 + Record_Alignment - 1)
      / Record_Alignment * Record_Alignment)
   with Post => Record_Bytes'Result mod Record_Alignment = 0
                and then Record_Bytes'Result <= Largest_Dirent;

   --  Write the entry at Offset of a checked page (Limit: its bytes used)
   --  at Buffer (Used ..) if it fits. OK False: the entry is malformed
   --  (nothing written). Next: the following entry's offset.
   procedure Encode
     (P : DP.Page; Offset, Limit : Natural; Buffer : in out Bytes; Used : in out Natural;
      Fits, OK : out Boolean; Next : out Natural)
   with Pre => Buffer'First = 0
               and then Buffer'Last in -1 .. Maximum_Buffer_Bytes - 1
               and then Used <= Buffer'Last + 1,
        Post => Used <= Buffer'Last + 1
                and then (if not (Fits and OK) then Used = Used'Old);

end CuBit.Libc_Directory_Entries;
