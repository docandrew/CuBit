------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.* and archive.* in the shared host environment (SELF-003 step 1):
--  typed requests checked here, the work done by CCL_Fetch and
--  CCL_Archive (one body per platform).
------------------------------------------------------------------------------
with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL_Stream_Table;

package CCL_Net_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value; Now_Ms : Interfaces.Unsigned_64;
      Streams : in out CCL_Stream_Table.Table; Reply : out CCL.Host_Values.Call_Result);
   --  Completes the reply task of each fetch that ended. Arrived: one did.
   procedure Pump
     (Streams : in out CCL_Stream_Table.Table; Now_Ms : Interfaces.Unsigned_64; Arrived : out Boolean);
   function Running return Boolean;
   --  Milliseconds until a running fetch's deadline (Unsigned_64'Last: none).
   function Next_Due return Interfaces.Unsigned_64;

   --  Places as the requests name them (git.* checks its own the same way).
   --  One entry's name: printable ASCII, no separator, not "." or "..".
   function Valid_Name (Name : String) return Boolean;
   --  A place's path below its root: "" or names joined by single '/'.
   function Valid_Below (Path : String) return Boolean;
   --  A place's root: a volume or alias ("@system", "@nvme:0"), then names.
   function Valid_Root (Root : String) return Boolean;
end CCL_Net_Bindings;
