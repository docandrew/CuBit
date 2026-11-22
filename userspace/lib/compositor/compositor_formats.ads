with System;
with Interfaces;
package Compositor_Formats with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_32;
   subtype Byte_Count is Interfaces.Unsigned_64;
   use type Word, Byte_Count, System.Address;
   type Image is record
      Pixels : System.Address := System.Null_Address;
      Width, Height, Pitch, Writable : Word := 0;
   end record with Convention => C;
   type Draw is record
      SX, SY, SW, SH, DX, DY, DW, DH : Word := 0;
      Clip_X, Clip_Y, Clip_W, Clip_H, Over : Word := 0;
   end record with Convention => C;
   function Valid (I : Image; Capacity : Byte_Count) return Boolean is
     (I.Pixels /= System.Null_Address and then
      I.Width in 1 .. 4096 and then I.Height in 1 .. 4096 and then
      I.Pitch in I.Width * 4 .. 16384 and then I.Pitch mod 4 = 0 and then
      I.Writable <= 1 and then Capacity >= Byte_Count (I.Pitch) * Byte_Count (I.Height));
   function Fits (D : Draw; Source, Target : Image) return Boolean is
     (Target.Writable = 1 and then Source.Pixels /= Target.Pixels and then
      D.SX < Source.Width and then D.SY < Source.Height and then
      D.SW in 1 .. Source.Width - D.SX and then
      D.SH in 1 .. Source.Height - D.SY and then
      D.DX < Target.Width and then D.DY < Target.Height and then
      D.DW in 1 .. Target.Width - D.DX and then
      D.DH in 1 .. Target.Height - D.DY and then
      D.Clip_X >= D.DX and then D.Clip_Y >= D.DY and then
      D.Clip_X - D.DX < D.DW and then D.Clip_Y - D.DY < D.DH and then
      D.Clip_W in 1 .. D.DW - (D.Clip_X - D.DX) and then
      D.Clip_H in 1 .. D.DH - (D.Clip_Y - D.DY) and then D.Over <= 1);
   --  Valid/Fits check numeric layout, not actual mapping authority or physical
   --  aliasing. Those remain existing grant/output-owner obligations.
end Compositor_Formats;
