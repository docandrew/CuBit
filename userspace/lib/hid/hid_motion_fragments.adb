pragma Ada_2022;
package body HID_Motion_Fragments with SPARK_Mode is
   function Part (Value : Motion) return Pointer_Pending.Displacement is
     (if Value > Motion (Pointer_Pending.Displacement'Last)
      then Pointer_Pending.Displacement'Last
      elsif Value < Motion (Pointer_Pending.Displacement'First)
      then Pointer_Pending.Displacement'First
      else Pointer_Pending.Displacement (Value));
   procedure Take
     (Remaining : in out Frame; Before_Buttons : Pointer_Pending.Button_State;
      Data : out Pointer_Pending.Report; Finished : out Boolean) is
   begin
      Data := (Buttons => Before_Buttons, X => Part (Remaining.X),
        Y => Part (Remaining.Y), Wheel => 0, Flags => Remaining.Flags);
      Remaining.X := Remaining.X - Motion (Data.X);
      Remaining.Y := Remaining.Y - Motion (Data.Y);
      Finished := Remaining.X = 0 and then Remaining.Y = 0;
      if Finished then
         Data.Buttons := Remaining.Buttons;
         Data.Wheel := Remaining.Wheel;
         Remaining.Wheel := 0;
      end if;
   end Take;
end HID_Motion_Fragments;
