-------------------------------------------------------------------------------
 -- CuBitOS
 -- Copyright (C) 2019 Jon Andrew
 --
 -- Ada Exception Handling
-------------------------------------------------------------------------------
with CuBit.Messages;
with System.Address_Image;

package body Ada.Exceptions.Last_Chance_Handler with
   SPARK_Mode => On
is
   --------------------------
   -- Last_Chance_Handler  --
   --------------------------
   procedure Last_Chance_Handler (msg : System.Address; line : Integer)
   with
      SPARK_Mode => On
   is
      use ASCII;
   begin
      CuBit.Messages.debugPrint ("User-space exception at " &
         System.Address_Image (msg) & " line " & line'Image & LF);
   end Last_Chance_Handler;

end Ada.Exceptions.Last_Chance_Handler;
