------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . P O O L _ G L O B A L                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  This is provided to get GNAT to allow "storage pools", but in CuBit
--  we only use GNAT's Simple_Storage_Pool facility.

with System;
with System.Storage_Pools;
with System.Storage_Elements;

package System.Pool_Global is
   pragma Elaborate_Body;

   type Unbounded_No_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with null record;

   overriding function Storage_Size
     (Pool : Unbounded_No_Reclaim_Pool)
      return System.Storage_Elements.Storage_Count;

   overriding procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
       Alignment    : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   --  Pool object used by the compiler when implicit Storage Pool objects are
   --  explicitly referred to. For instance when writing something like:
   --     for T'Storage_Pool use Q'Storage_Pool;
   --  and Q'Storage_Pool hasn't been defined explicitly.

   Global_Pool_Object : aliased Unbounded_No_Reclaim_Pool;

end System.Pool_Global;
