------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Copy a frame's damage boxes from one 32-bit image to another (LAT-002
--  multi-rect submit): display.svc copies only the boxes a frame names
--  (CuBit.Display_Frame_Damage), row by row, never their envelope.
--  The caller has validated every box against both images.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces;
with CuBit.Display_Frame_Damage;
package CuBit.Display_Box_Copy is
   use type Interfaces.Unsigned_64;
   package FD renames CuBit.Display_Frame_Damage;
   --  The bytes one box covers (for the copy counters).
   function Bytes (Box : FD.DP.Rectangle) return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (Box.Width) * Interfaces.Unsigned_64 (Box.Height) * 4);
   procedure Copy_Box
     (Target : System.Address; Target_Pitch : Natural;
      Source : System.Address; Source_Pitch : Natural;
      Box : FD.DP.Rectangle);
   --  Every box of Item; the bytes copied in Copied.
   procedure Copy_Boxes
     (Target : System.Address; Target_Pitch : Natural;
      Source : System.Address; Source_Pitch : Natural;
      Item : FD.Damage; Copied : out Interfaces.Unsigned_64);
end CuBit.Display_Box_Copy;
