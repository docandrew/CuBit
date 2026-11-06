------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a masked text field shows in place of its secret, and where its caret
--  and selection go (CuBit.UI.Draw_Text_Edit_Field). Pure: no pixels.
------------------------------------------------------------------------------
package CuBit.UI_Masking with SPARK_Mode, Pure is
   --  Fixed_Length hides the length too; Per_Character shows one mask
   --  character per character of the secret (user, 2026-11-05: Wi-Fi).
   type Style is (Fixed_Length, Per_Character);

   Fixed_Mask : constant String := "*******";
   Mask_Character : constant Character := '*';

   --  How many mask characters stand in for a secret of Length characters.
   function Shown_Length (Length : Natural; Mask : Style) return Natural is
     (if Length = 0 then 0
      elsif Mask = Per_Character then Length
      else Fixed_Mask'Length)
   with Post => (if Length = 0 then Shown_Length'Result = 0)
                and then (if Mask = Per_Character then Shown_Length'Result = Length)
                and then (if Mask = Fixed_Length and Length > 0
                          then Shown_Length'Result = Fixed_Mask'Length);

   --  A text position (0-based caret or selection end) in the shown mask.
   --  Per_Character keeps it where it is; Fixed_Length puts it after the mask.
   function Shown_Position (Position, Length : Natural; Mask : Style) return Natural is
     (if Mask = Per_Character then Natural'Min (Position, Length)
      else Shown_Length (Length, Mask))
   with Post => Shown_Position'Result <= Shown_Length (Length, Mask);
end CuBit.UI_Masking;
