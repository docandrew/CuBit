------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Files in places for net.fetch and archive.unpack (SELF-003 step 1): the
--  filesystem service's message requests through one 64 KiB buffer lent
--  once. Every path is checked by the service against this program's
--  filesystem scope on every request; nothing here grants anything.
------------------------------------------------------------------------------
with Interfaces;
with System;

package CCL_Place_Files is
   use Interfaces;

   --  The buffer: one transfer's bytes, or a rename's two paths.
   BUFFER_BYTES : constant := 65_536;
   MAXIMUM_PATH_BYTES : constant := 4_096;

   type Result_Kind is
     (Done, Not_Found, Not_Granted, Already_Exists, Wrong_Kind, Not_Empty, No_Space, Unavailable, Failed);

   type File is private;
   No_File : constant File;

   --  A new file at Path, open for writing: Exclusive refuses one that
   --  exists (Already_Exists); otherwise it is truncated.
   procedure Create (Path : String; Exclusive : Boolean; Item : out File; Result : out Result_Kind);
   --  An existing file, open for reading.
   procedure Open_Read (Path : String; Item : out File; Result : out Result_Kind);
   --  Count bytes from Data at Offset of the file (any count; sent a
   --  buffer at a time).
   procedure Write
     (Item : File; Offset : Unsigned_64; Data : System.Address; Count : Natural; Result : out Result_Kind);
   --  Up to Count bytes from Offset into Data; Got < Count only at the end.
   procedure Read
     (Item : File; Offset : Unsigned_64; Data : System.Address; Count : Natural; Got : out Natural;
      Result : out Result_Kind);
   procedure Close (Item : in out File);

   procedure Make_Directory (Path : String; Result : out Result_Kind);
   procedure Remove_File (Path : String; Result : out Result_Kind);
   procedure Remove_Directory (Path : String; Result : out Result_Kind);
   --  Within one volume; To is replaced when it is a file.
   procedure Rename (From, To : String; Result : out Result_Kind);
   --  Whether anything (a file or a directory) is at Path.
   function Exists (Path : String) return Boolean;
private
   type File is record
      Handle : Unsigned_64 := 0;
      Open : Boolean := False;
   end record;
   No_File : constant File := (others => <>);
end CCL_Place_Files;
