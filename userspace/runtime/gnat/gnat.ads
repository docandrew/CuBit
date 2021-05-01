------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                                 G N A T                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1992-2019, AdaCore                     --
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

--  This is the parent package for a library of useful units provided with GNAT

package GNAT is
   pragma Pure;

   --  The following type denotes the range of buckets for various hashed
   --  data structures in the GNAT unit hierarchy.

   type Bucket_Range_Type is mod 2 ** 32;

   --  The following exception is raised whenever an attempt is made to mutate
   --  the state of a data structure that is being iterated on.

   Iterated : exception;

   --  The following exception is raised when an iterator is exhausted and
   --  further attempts are made to advance it.

   Iterator_Exhausted : exception;

   --  The following exception is raised whenever an attempt is made to mutate
   --  the state of a data structure that has not been created yet.

   Not_Created : exception;

end GNAT;
