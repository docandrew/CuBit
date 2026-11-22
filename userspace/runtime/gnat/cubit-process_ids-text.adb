------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Process_IDs.Text with SPARK_Mode is
   function Image (Process : Process_ID) return String is
      Text : Image_Text;
      Last : Positive;
   begin
      CuBit.Process_IDs.Image (Process, Text, Last);
      return Text (1 .. Last);
   end Image;
end CuBit.Process_IDs.Text;
