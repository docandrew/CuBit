------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A process as text, for programs that may use the secondary stack
--  (CuBit.Process_IDs.Image is the allocation-free form).
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Process_IDs.Text with Pure, SPARK_Mode is
   --  Decimal: the number ps shows and kill reads back (Parse).
   function Image (Process : Process_ID) return String;
end CuBit.Process_IDs.Text;
