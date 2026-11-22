-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with Ada.Exceptions;
with Host_Signing;

package body Build_Signing is

   procedure Sign (Directory, Input, Output : String) is
      Signer : Host_Signing.Signer;
      Data : Host_Signing.Bytes_Access;
   begin
      Host_Signing.Load (Signer, Directory);
      Data := Host_Signing.Read_File (Input);
      if Host_Signing.Has_Footer (Data.all) then
         Host_Signing.Free (Data);
         raise Signing_Unavailable with Input & " already carries a signature";
      end if;
      Host_Signing.Write_File
        (Output, Data.all, Host_Signing.Executable_File, Host_Signing.Sign (Signer, Data.all));
      Host_Signing.Free (Data);
   exception
      when E : Host_Signing.Signing_Error =>
         Host_Signing.Free (Data);
         raise Signing_Unavailable with Ada.Exceptions.Exception_Message (E);
   end Sign;

end Build_Signing;
