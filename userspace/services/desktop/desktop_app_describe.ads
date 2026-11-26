------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Who a window's owner is, for its task button (UI-061): procmgr's
--  Describe (CuBit.App_Names), asked asynchronously, one at a time.
--
--  @description
--  procmgr answers with the app name and identity the owner's executable
--  declared in its signed identity section. Desktop submits and takes the
--  completion in its event loop; it never waits for procmgr (procmgr never
--  calls Desktop, so the request goes down the IPC graph). The answer is
--  checked again here (CuBit.App_Names.Valid_Name) before Desktop shows it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CuBit.App_Names;
with CuBit.Messages;
with CuBit.Process_IDs;

package Desktop_App_Describe is
   package AN renames CuBit.App_Names;

   --  Lend procmgr one page once; False until that worked.
   procedure Initialize;
   --  A request may be submitted now.
   function Ready return Boolean;
   --  procmgr could not be lent the page: no request will ever be made.
   function Unavailable return Boolean;
   procedure Submit
     (Owner : CuBit.Process_IDs.Process_ID; Sequence : in out Interfaces.Unsigned_64; Submitted : out Boolean);
   --  The request in flight (0: none) and whom it asks about.
   function Token return Interfaces.Unsigned_64;

   type Answer is record
      Owner : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
      --  procmgr launched it and answered (the name may still be empty).
      Known : Boolean := False;
      Name : String (1 .. AN.Maximum_Name_Bytes) := [others => ' '];
      Name_Length : AN.Name_Length := 0;
      Identity : String (1 .. AN.Maximum_Identity_Bytes) := [others => ' '];
      Identity_Length : AN.Identity_Length := 0;
   end record;
   procedure Collect (C : CuBit.Messages.CompletionEntry; Result : out Answer);
end Desktop_App_Describe;
