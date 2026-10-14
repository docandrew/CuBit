------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's authority (CAP-002): the roots the kernel placed in its table
--  before it ran (CuBit.Handles.Boot_Roots), and which device-memory root
--  covers a physical range.
--
--  @description
--  Every capability devmgr hands a child is a narrowed copy of one of these
--  roots or a kernel object SPAWN gave it; devmgr itself maps device pages
--  and programs hardware through them.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Boot_Roots;

package Devmgr_Authority is

   package H renames CuBit.Handles;

   --  Read the roots from devmgr's launch configuration and the extent of
   --  each device-memory root. Valid False when devmgr was not given them.
   procedure Initialize;
   function Valid return Boolean;
   function Roots return CuBit.Handles.Boot_Roots.Roots;

   --  The device-memory root covering [Physical, Physical + Bytes), or the
   --  null handle.
   function Device_Region (Physical, Bytes : Unsigned_64) return H.Device_Memory;

   --  That range of the covering root, with Kept rights (a copy for a
   --  child). Covered tells whether one covers it.
   function Covered (Physical, Bytes : Unsigned_64) return Boolean is
     (H.Valid (Device_Region (Physical, Bytes)));
   function Device_Copy (Physical, Bytes : Unsigned_64; Kept : H.Rights) return H.Derivation
   with Pre => Covered (Physical, Bytes);

end Devmgr_Authority;
