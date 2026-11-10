------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Access types of a package whose default pool is CuAlloc's, by the
--  package-spec form of pragma Default_Storage_Pool (RM 13.11.3).
------------------------------------------------------------------------------
pragma Ada_2022;
pragma Warnings (Off, "no entities of * are referenced");
with CuBit.CuAlloc_Pool;
pragma Warnings (On, "no entities of * are referenced");
--  (GNAT does not count the pragma Default_Storage_Pool reference)
with Shapes;

package Pooled is
   pragma Default_Storage_Pool (CuBit.CuAlloc_Pool.Pool);

   type Integer_Access is access Integer;
   type String_Access is access String;
   type Shape_Access is access Shapes.Shape'Class;

end Pooled;
