with Interfaces;
--  Absolute-pointer reports: tablets, touch screens and virtual machine
--  pointers (QEMU virtio-tablet), whose device reports where the pointer IS
--  rather than how far it moved.
--
--  ABSOLUTE_POINTER source reports travel as REPLACEABLE_STATE (CuBit.Input):
--  a newer position replaces an older one, while button and device-flag
--  transitions stay ORDERED (Absolute_Pending never merges across them).
--
--  Positions are normalized by the driver to 0 .. Axis_Last on each axis,
--  both edges included. Axis_Last equals QEMU's INPUT_EVENT_ABS_MAX, so a
--  QEMU tablet's values pass through unchanged. The consumer maps a position
--  onto the native pixels of the output the pointer is bound to (To_Pixel),
--  then into desktop coordinates with that output's scale and rotation.
package Absolute_Pointer with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16,
     Interfaces.Unsigned_64;
   subtype Word is Interfaces.Unsigned_64;

   Axis_Bits : constant := 15;
   Axis_Last : constant := 2 ** Axis_Bits - 1;
   subtype Axis_Position is Interfaces.Unsigned_16 range 0 .. Axis_Last;

   Wheel_Bits : constant := 8;
   Wheel_Bias : constant := 2 ** (Wheel_Bits - 1);
   subtype Wheel_Steps is Integer range -Wheel_Bias .. Wheel_Bias - 1;
   subtype Button_State is Interfaces.Unsigned_8;
   subtype Device_Flags is Interfaces.Unsigned_8;

   --  Wire payload of an ABSOLUTE_POINTER source report:
   --  bits 0..7 buttons (bit 0 left, 1 right, 2 middle, 3 side, 4 extra),
   --  8..23 X, 24..39 Y (each at most Axis_Last), 40..47 wheel (excess
   --  Wheel_Bias: the field minus 128 is the step count, positive away from
   --  the user), 48..55 device flags, 56..63 zero.
   Byte_Mask   : constant Word := 16#FF#;
   Axis_Mask   : constant Word := 16#FFFF#;
   X_Shift     : constant := 8;
   Y_Shift     : constant := 24;
   Wheel_Shift : constant := 40;
   Flags_Shift : constant := 48;
   Reserved_Shift : constant := 56;

   type Report is record
      Buttons : Button_State := 0;
      X, Y    : Axis_Position := 0;
      Wheel   : Wheel_Steps := 0;
      Flags   : Device_Flags := 0;
   end record;

   function Same_Position (A, B : Report) return Boolean is
     (A.X = B.X and then A.Y = B.Y);

   function Wheel_Field (Value : Wheel_Steps) return Interfaces.Unsigned_8 is
     (Interfaces.Unsigned_8 (Value + Wheel_Bias));

   function Wheel_Value (Field : Interfaces.Unsigned_8) return Wheel_Steps is
     (Integer (Field) - Wheel_Bias);

   function Encode (R : Report) return Word is
     (Word (R.Buttons) or
      Interfaces.Shift_Left (Word (R.X), X_Shift) or
      Interfaces.Shift_Left (Word (R.Y), Y_Shift) or
      Interfaces.Shift_Left (Word (Wheel_Field (R.Wheel)), Wheel_Shift) or
      Interfaces.Shift_Left (Word (R.Flags), Flags_Shift));

   --  Untrusted wire words: both axes in range, reserved bits clear.
   function Valid_Payload (W : Word) return Boolean is
     ((Interfaces.Shift_Right (W, X_Shift) and Axis_Mask) <= Axis_Last and then
      (Interfaces.Shift_Right (W, Y_Shift) and Axis_Mask) <= Axis_Last and then
      Interfaces.Shift_Right (W, Reserved_Shift) = 0);

   function Decode (W : Word) return Report is
     ((Buttons => Button_State (W and Byte_Mask),
       X       => Axis_Position (Interfaces.Shift_Right (W, X_Shift) and Axis_Mask),
       Y       => Axis_Position (Interfaces.Shift_Right (W, Y_Shift) and Axis_Mask),
       Wheel   => Wheel_Value (Interfaces.Unsigned_8
                    (Interfaces.Shift_Right (W, Wheel_Shift) and Byte_Mask)),
       Flags   => Device_Flags (Interfaces.Shift_Right (W, Flags_Shift) and Byte_Mask)))
     with Pre => Valid_Payload (W);

   --  Every report encodes to a valid payload that decodes to itself, and
   --  every valid payload is the encoding of what it decodes to: the codec
   --  is a bijection between reports and valid payloads.
   procedure Lemma_Encode_Decode (R : Report)
     with Ghost, Always_Terminates,
       Post => Valid_Payload (Encode (R)) and then Decode (Encode (R)) = R;
   procedure Lemma_Decode_Encode (W : Word)
     with Ghost, Always_Terminates,
       Pre => Valid_Payload (W), Post => Encode (Decode (W)) = W;

   --  A device's reported axis range (virtio-input ABS_INFO minimum and
   --  maximum, Linux input_absinfo).
   subtype Device_Value is Long_Long_Integer range -(2 ** 31) .. 2 ** 31 - 1;
   type Device_Range is record
      Minimum, Maximum : Device_Value := 0;
   end record;
   function Valid_Range (R : Device_Range) return Boolean is
     (R.Minimum < R.Maximum);

   --  Scale a device value onto the normalized axis, clamping values the
   --  device reports outside its own range. Both edges map to both edges.
   function Normalize (Value : Device_Value; R : Device_Range)
     return Axis_Position
   is (if Value <= R.Minimum then 0
       elsif Value >= R.Maximum then Axis_Last
       else Axis_Position
         ((Value - R.Minimum) * Axis_Last / (R.Maximum - R.Minimum)))
     with Pre => Valid_Range (R),
       Post => (if Value <= R.Minimum then Normalize'Result = 0) and then
               (if Value >= R.Maximum then Normalize'Result = Axis_Last);

   --  Native pixels on one axis of the output the pointer is bound to.
   Maximum_Extent : constant := 65_535;
   subtype Pixel_Extent is Positive range 1 .. Maximum_Extent;

   --  The normalized position QEMU sends for host pixel Pixel of a guest
   --  surface Extent pixels wide (ui/input.c qemu_input_scale_axis with
   --  min_in 0, max_in Extent). Used to model hosts and to inject input.
   function Host_Position (Pixel : Natural; Extent : Pixel_Extent)
     return Axis_Position is
     (Axis_Position (Pixel * Axis_Last / Extent))
     with Pre => Pixel < Extent;

   --  The native pixel a normalized position points at: the least pixel
   --  whose Host_Position reaches it (a ceiling division), so for extents
   --  up to Axis_Last it exactly inverts Host_Position. Never outside the
   --  output, and the edges map to the edges.
   function To_Pixel (Position : Axis_Position; Extent : Pixel_Extent)
     return Natural is
     (Natural'Min (Extent - 1,
                   (Natural (Position) * Extent + Axis_Last - 1) / Axis_Last))
     with Post => To_Pixel'Result < Extent and then
       (if Position = 0 then To_Pixel'Result = 0) and then
       (if Position = Axis_Last then To_Pixel'Result = Extent - 1);

   --  For an output no wider than the axis, To_Pixel recovers the exact
   --  pixel a QEMU host pointer is over: no drift, no off-by-one.
   procedure Lemma_Exact_Inverse (Pixel : Natural; Extent : Pixel_Extent)
     with Ghost, Always_Terminates,
       Pre => Extent <= Axis_Last and then Pixel < Extent,
       Post => To_Pixel (Host_Position (Pixel, Extent), Extent) = Pixel;

   --  Moving the device right (down) never moves the pointer left (up).
   procedure Lemma_To_Pixel_Monotone
     (A, B : Axis_Position; Extent : Pixel_Extent)
     with Ghost, Always_Terminates,
       Pre => A <= B, Post => To_Pixel (A, Extent) <= To_Pixel (B, Extent);
end Absolute_Pointer;
