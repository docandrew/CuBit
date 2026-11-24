------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git.* in the shared host environment (SELF-003 step 2): typed requests
--  checked here, the clone done by CCL_Git (Git_Clone_Engine on CuBit).
------------------------------------------------------------------------------
with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL_Stream_Table;

package CCL_Git_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value; Now_Ms : Interfaces.Unsigned_64;
      Streams : in out CCL_Stream_Table.Table; Reply : out CCL.Host_Values.Call_Result);
   --  Completes the reply task of each clone that ended. Arrived: one did.
   procedure Pump
     (Streams : in out CCL_Stream_Table.Table; Now_Ms : Interfaces.Unsigned_64; Arrived : out Boolean);
   function Running return Boolean;
   --  Milliseconds until a running clone's deadline (Unsigned_64'Last: none).
   function Next_Due return Interfaces.Unsigned_64;
end CCL_Git_Bindings;
