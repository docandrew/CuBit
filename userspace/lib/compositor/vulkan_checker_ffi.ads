with System;
with Interfaces;
package Vulkan_Checker_FFI with SPARK_Mode is
   subtype Signed is Interfaces.Integer_32;
   subtype Word is Interfaces.Unsigned_32;
   type Request is record
      Left, Top, Right, Bottom, Origin_X, Origin_Y : Signed;
      Numerator, Denominator, Width, Height, Rotation : Word;
      Clip_X, Clip_Y, Clip_W, Clip_H, RGB : Word;
   end record with Convention => C;
   -- Borrowed is the exact owned submission, not a source/image pointer.
   -- The device adapter authenticates it and keeps its checker pipeline alive.
   -- Zero records commands only. Nonzero rejects the whole unsubmitted frame.
   procedure Record_Draw (Borrowed : System.Address; Value : Request; Result : out Word)
     with Global => null;
end Vulkan_Checker_FFI;
