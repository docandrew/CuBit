------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  archive.unpack (CCL.Interfaces.Archive), one body per platform: native/
--  runs the proved tar reader's unpacking (Tar_Unpack) over the filesystem
--  service, which checks every path against this program's scope; the
--  Linux preview's host/ refuses (Unavailable), a stand-in, not CuBit.
------------------------------------------------------------------------------
with Interfaces;

package CCL_Archive is
   use Interfaces;

   MAXIMUM_TEXT : constant := 4_096;

   --  How an unpack ended, in CCL's Unpack_Outcome order.
   type Outcome is (Unpacked, Not_Granted, Not_Found, Exists, Refused, Too_Large, Write_Failed);

   type Text is record
      Data : String (1 .. MAXIMUM_TEXT) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_TEXT := 0;
   end record;
   function To_Text (Item : String) return Text;

   type Report is record
      Ended_As : Outcome := Not_Found;
      Entries, Bytes : Unsigned_64 := 0;
      Skipped : Natural := 0;
      Reason, Entry_Name, Detail : Text;
   end record;

   --  From_Root/From_Path/Name, unpacked as Into_Root/Into_Path/Directory.
   --  Available False: this platform cannot (Result.Detail says why).
   procedure Unpack
     (From_Root, From_Path, Name, Into_Root, Into_Path, Directory : String; Skip_Specials : Boolean;
      Max_Entries : Natural; Max_Bytes : Unsigned_64; Result : out Report; Available : out Boolean);
end CCL_Archive;
