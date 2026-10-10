package body Intel_GPU_Cursor_Program with SPARK_Mode is
   function Encode_Position (X, Y : Position) return Unsigned_32 is
     (Magnitude (X) or Shift_Left (Magnitude (Y), Y_Shift) or
      (if X < 0 then X_Sign else 0) or (if Y < 0 then Y_Sign else 0));

   function Encode
     (Item : Image; X, Y : Position; Version : Display_Version;
      VTd_Workaround : Boolean) return Register_Values
   is
     ((FBC_Control =>
         (if Item.Height = Pixels (Item.Width) then 0
          else FBC_Enable or Unsigned_32 (Item.Height - 1)),
       Control =>
         Mode (Item.Width) or
           (if Version = Arbitration_Workaround_Version
            then Shift_Left (Arbitration_Slots, Arbitration_Shift) else 0),
       Position_Word => Encode_Position (X, Y),
       Base => Item.Base));
end Intel_GPU_Cursor_Program;
