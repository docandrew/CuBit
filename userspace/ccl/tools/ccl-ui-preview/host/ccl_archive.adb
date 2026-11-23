------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The Linux preview does not unpack: a Linux-hosted stand-in, not CuBit
--  (tests/tar-reader checks the same unpacking over host directories).
------------------------------------------------------------------------------
package body CCL_Archive is
   function To_Text (Item : String) return Text is
      N : constant Natural := Natural'Min (Item'Length, MAXIMUM_TEXT);
      Result : Text;
   begin
      Result.Data (1 .. N) := Item (Item'First .. Item'First + N - 1);
      Result.Length := N;
      return Result;
   end To_Text;

   procedure Unpack
     (From_Root, From_Path, Name, Into_Root, Into_Path, Directory : String; Skip_Specials : Boolean;
      Max_Entries : Natural; Max_Bytes : Unsigned_64; Result : out Report; Available : out Boolean)
   is
      pragma Unreferenced (From_Root, From_Path, Name, Into_Root, Into_Path, Directory, Skip_Specials,
                           Max_Entries, Max_Bytes);
   begin
      Result := (Detail => To_Text ("the Linux preview does not unpack archives"), others => <>);
      Available := False;
   end Unpack;
end CCL_Archive;
