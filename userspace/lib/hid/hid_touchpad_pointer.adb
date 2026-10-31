pragma Ada_2022;
package body HID_Touchpad_Pointer with SPARK_Mode is
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type Pointer_Pending.Button_State;
   Empty_Report : constant Pointer_Pending.Report := (Buttons => 0, X => 0, Y => 0, Wheel => 0, Flags => 0);
   Wire_Motion_Bound : constant Motion_Math := Motion_Math
     (Integer'Min (Pointer_Pending.Displacement'Last, -Pointer_Pending.Displacement'First));
   function Admissible (Range_Value : Axis_Range; Scale : Gain) return Boolean is
      Distance, Numerator, Denominator : Motion_Math;
   begin
      if Range_Value.Minimum >= Range_Value.Maximum then return False; end if;
      Distance := Motion_Math (Range_Value.Maximum) - Motion_Math (Range_Value.Minimum);
      Numerator := Distance * Motion_Math (Scale.Numerator); Denominator := Motion_Math (Scale.Denominator);
      return (Numerator + Denominator - 1) / Denominator <= Wire_Motion_Bound;
   end Admissible;
   procedure Clear_Contact (S : in out State) is
   begin
      S.Has_Primary := False; S.ID := 0; S.Last_X := 0; S.Last_Y := 0; S.Remainder_X := 0; S.Remainder_Y := 0;
   end Clear_Contact;
   procedure Rebase (S : in out State) is
   begin Clear_Contact (S); end Rebase;
   procedure Reset (S : in out State; Result : out Decision) is
   begin
      Result := (Outcome => Reset_Done, Emit => S.Buttons /= 0, Data => Empty_Report);
      Clear_Contact (S); S.Buttons := 0;
   end Reset;
   procedure Configure
     (S : in out State; X_Range, Y_Range : Axis_Range;
      X_Gain, Y_Gain : Gain; X_Direction, Y_Direction : Direction;
      Confidence : Confidence_Policy; Result : out Decision) is
   begin
      Reset (S, Result); S.Configured := False; Result.Outcome := Invalid_Configuration;
      if X_Range.Minimum >= X_Range.Maximum or else Y_Range.Minimum >= Y_Range.Maximum then return; end if;
      if not Admissible (X_Range, X_Gain) or else not Admissible (Y_Range, Y_Gain) then Result.Outcome := Scale_Refused; return; end if;
      S.X_Range := X_Range; S.Y_Range := Y_Range; S.X_Gain := X_Gain; S.Y_Gain := Y_Gain;
      S.X_Direction := X_Direction; S.Y_Direction := Y_Direction; S.Confidence := Confidence;
      S.Configured := True; Result.Outcome := Policy_Configured;
   end Configure;
   procedure Update
     (S : in out State; Samples : Contact_Array;
      Known_Buttons, Pressed_Buttons : Pointer_Pending.Button_State;
      Result : out Decision)
   is
      Selected : Boolean := False;
      ID : Interfaces.Unsigned_32 := 0;
      X, Y : Coordinate := 0;
      Raw_X, Raw_Y : Long_Long_Integer;
      Candidate_ID : Interfaces.Unsigned_32;
      Known, Certain, Eligible : Boolean;
      Old_Buttons : constant Pointer_Pending.Button_State := S.Buttons;
      Buttons : Pointer_Pending.Button_State;
      DX, DY : Motion_Math := 0;
      Rem_X, Rem_Y : Motion_Math := 0;
      procedure Refuse is
      begin Reset (S, Result); Result.Outcome := Invalid_Frame; end Refuse;
      procedure Scale_Delta
        (Delta_Value : Motion_Math; Scale : Gain; Orientation : Direction;
         Previous : Motion_Math; Displacement, Remainder : out Motion_Math)
      is
         Total : Motion_Math := Delta_Value * Motion_Math (Scale.Numerator);
      begin
         if Orientation = Reversed then Total := -Total; end if;
         Total := Total + Previous;
         Displacement := Total / Motion_Math (Scale.Denominator);
         Remainder := Total rem Motion_Math (Scale.Denominator);
      end Scale_Delta;
   begin
      Result := (Outcome => Policy_Not_Configured, Emit => False, Data => Empty_Report);
      if not S.Configured then return; end if;
      if Interfaces.Unsigned_64 (Samples'Length) > Interfaces.Unsigned_64 (Maximum_Contacts) or else
         (Pressed_Buttons and not Known_Buttons) /= 0 then Refuse; return; end if;
      for N in Samples'Range loop
         Candidate_ID := Identity (Samples (N));
         for J in Samples'First .. N - 1 loop
            if Candidate_ID = Identity (Samples (J)) then Refuse; return; end if;
         end loop;
         Known := Confidence_Known (Samples (N)); Certain := Confident (Samples (N));
         if Certain and then not Known then Refuse; return; end if;
         if Active (Samples (N)) and then not Positioned (Samples (N)) then Refuse; return; end if;
         Eligible := Active (Samples (N)) and then Positioned (Samples (N)) and then
           ((Known and then Certain) or else (not Known and then S.Confidence = Accept_Unknown));
         if Eligible then
            Raw_X := X_Value (Samples (N)); Raw_Y := Y_Value (Samples (N));
            if Raw_X < Long_Long_Integer (S.X_Range.Minimum) or else Raw_X > Long_Long_Integer (S.X_Range.Maximum) or else
               Raw_Y < Long_Long_Integer (S.Y_Range.Minimum) or else Raw_Y > Long_Long_Integer (S.Y_Range.Maximum)
            then Refuse; return; end if;
            if not Selected or else (S.Has_Primary and then Candidate_ID = S.ID) then
               Selected := True; ID := Candidate_ID; X := Coordinate (Raw_X); Y := Coordinate (Raw_Y);
            end if;
         end if;
      end loop;
      Buttons := (Old_Buttons and not Known_Buttons) or Pressed_Buttons;
      if Selected then
         if S.Has_Primary and then ID = S.ID then
            Scale_Delta (Motion_Math (X) - Motion_Math (S.Last_X), S.X_Gain, S.X_Direction, S.Remainder_X, DX, Rem_X);
            Scale_Delta (Motion_Math (Y) - Motion_Math (S.Last_Y), S.Y_Gain, S.Y_Direction, S.Remainder_Y, DY, Rem_Y);
         end if;
         if DX not in Motion_Math (Pointer_Pending.Displacement'First) .. Motion_Math (Pointer_Pending.Displacement'Last) or else
            DY not in Motion_Math (Pointer_Pending.Displacement'First) .. Motion_Math (Pointer_Pending.Displacement'Last)
         then Refuse; return; end if;
         S.Has_Primary := True; S.ID := ID; S.Last_X := X; S.Last_Y := Y; S.Remainder_X := Rem_X; S.Remainder_Y := Rem_Y;
      else Clear_Contact (S);
      end if;
      S.Buttons := Buttons;
      Result.Data := (Buttons => Buttons, X => Integer (DX), Y => Integer (DY), Wheel => 0, Flags => 0);
      Result.Emit := Buttons /= Old_Buttons or else DX /= 0 or else DY /= 0;
      Result.Outcome := (if Result.Emit then Pointer_Changed else Pointer_Unchanged);
   end Update;
end HID_Touchpad_Pointer;
