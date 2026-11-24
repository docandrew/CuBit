pragma Ada_2022;
with Pointer_Pending;
package HID_Motion_Fragments with SPARK_Mode, Pure is
   use type Pointer_Pending.Button_State;
   -- Wide completed-frame motion, independent of the wire representation.
   type Motion is range -(2 ** 62) .. 2 ** 62 - 1;
   type Frame is record
      Buttons : Pointer_Pending.Button_State := 0;
      X, Y : Motion := 0;
      Wheel : Pointer_Pending.Wheel_Steps := 0;
      Flags : Pointer_Pending.Device_Flags := 0;
   end record;
   procedure Take
     (Remaining : in out Frame; Before_Buttons : Pointer_Pending.Button_State;
      Data : out Pointer_Pending.Report; Finished : out Boolean)
     with Post =>
       Long_Long_Integer (Remaining.X'Old) =
         Long_Long_Integer (Remaining.X) + Long_Long_Integer (Data.X)
       and then Long_Long_Integer (Remaining.Y'Old) =
         Long_Long_Integer (Remaining.Y) + Long_Long_Integer (Data.Y)
       and then Remaining.Wheel'Old = Remaining.Wheel + Data.Wheel
       and then Remaining.Buttons = Remaining.Buttons'Old
       and then Remaining.Flags = Remaining.Flags'Old
       and then Data.Flags = Remaining.Flags
       and then Finished = (Remaining.X = 0 and then Remaining.Y = 0)
       and then Data.Buttons =
         (if Finished then Remaining.Buttons else Before_Buttons)
       and then (if Finished then Remaining.Wheel = 0 else Data.Wheel = 0)
       and then (if Remaining.X'Old >= 0 then Remaining.X in 0 .. Remaining.X'Old
                 else Remaining.X in Remaining.X'Old .. 0)
       and then (if Remaining.Y'Old >= 0 then Remaining.Y in 0 .. Remaining.Y'Old
                 else Remaining.Y in Remaining.Y'Old .. 0);
   -- Each step conserves both deltas and wheel motion exactly. Intermediate
   -- fragments retain the prior button state; the final fragment places the
   -- frame's transition and wheel at the completed motion endpoint. Zero-motion
   -- frames still produce one report, so button transitions are never omitted.
end HID_Motion_Fragments;
