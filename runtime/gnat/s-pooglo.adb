------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . P O O L _ G L O B A L                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--          Copyright (C) 2020, Jon Andrew                                  --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
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

with System.Storage_Pools; use System.Storage_Pools;

package body System.Pool_Global is

   package SSE renames System.Storage_Elements;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is
      NoGlobalPoolException : exception;
   begin
      raise NoGlobalPoolException with "No Global Storage Pool Available";
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   overriding procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is
      NoGlobalPoolException : exception;
   begin
      raise NoGlobalPoolException with "No Global Storage Pool Available";
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool  : Unbounded_No_Reclaim_Pool)
      return  SSE.Storage_Count
   is
      NoGlobalPoolException : exception;
   begin
      raise NoGlobalPoolException with "No Global Storage Pool Available";
      return 0;
   end Storage_Size;

end System.Pool_Global;
