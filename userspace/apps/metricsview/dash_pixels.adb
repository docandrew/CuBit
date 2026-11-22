pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Dash_Pixels is
   use type System.Address;
   Bytes_Per_Pixel : constant := 4;

   procedure Span (C : CuBit.UI.Canvas; X, Y, Height : Natural; Ink : CuBit.UI.Color) is
      R : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (C, (X, Y, 1, Height));
   begin
      if CuBit.UI.Is_Empty (R) then
         return;
      end if;
      if C.densityNumerator /= C.densityDenominator or else C.addr = System.Null_Address
        or else C.pitch mod Bytes_Per_Pixel /= 0 or else R.x >= C.pitch / Bytes_Per_Pixel
      then
         CuBit.UI.Fill_Rect (C, R, Ink);
         return;
      end if;
      declare
         Pitch : constant Natural := C.pitch / Bytes_Per_Pixel;
         type Pixels is array (0 .. (R.h - 1) * Pitch) of Unsigned_32;
         Target : Pixels with Import, Address => C.addr + Storage_Offset ((R.y * Pitch + R.x) * Bytes_Per_Pixel);
      begin
         for Row in 0 .. R.h - 1 loop
            Target (Row * Pitch) := Ink;
         end loop;
      end;
   end Span;
end Dash_Pixels;
