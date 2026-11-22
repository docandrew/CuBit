------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              S Y S T E M . S T A N D A R D _ L I B R A R Y               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1995-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  CuBit: the user runtime's body. No tasking and no abort (No_Tasking,
--  No_Abort_Statements), so there is no abort deferral to undo; and the
--  binder of a configurable run time emits no library-level finalization,
--  so Adafinal has nothing to run. The soft links upstream calls are not
--  part of this runtime.

package body System.Standard_Library is

   --------------------------
   -- Abort_Undefer_Direct --
   --------------------------

   procedure Abort_Undefer_Direct is
   begin
      null;
   end Abort_Undefer_Direct;

   --------------
   -- Adafinal --
   --------------

   procedure Adafinal is
   begin
      null;
   end Adafinal;

   -----------------
   -- Break_Start --
   -----------------

   procedure Break_Start;
   pragma Export (C, Break_Start, "__gnat_break_start");
   --  A convenient place for a debugger breakpoint (as upstream)

   procedure Break_Start is
   begin
      null;
   end Break_Start;

end System.Standard_Library;
