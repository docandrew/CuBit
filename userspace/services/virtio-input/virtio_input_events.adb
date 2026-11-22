pragma Ada_2022;
package body Virtio_Input_Events with SPARK_Mode is

   function Button_Of (Code : Interfaces.Unsigned_16) return Button_Bit is
     (Button_Bit (Code - BTN_LEFT));

   function Start (X, Y : Absolute_Pointer.Device_Range) return Assembler is
     ((X_Range => X, Y_Range => Y, Committed => (others => <>),
       Building => (others => <>), Changed => False, Discarding => False));

   procedure Feed
     (A : in out Assembler; E : Event; Result : out Outcome;
      Report : out Absolute_Pointer.Report)
   is
      use Absolute_Pointer;
   begin
      Report := A.Committed;
      Result := Absorbed;
      if E.Kind = EV_SYN then
         if E.Code = SYN_DROPPED then
            A.Discarding := True;
            A.Building := A.Committed;
            A.Changed := False;
            Result := Frame_Dropped;
         elsif E.Code = SYN_REPORT then
            if A.Discarding then
               --  Events of the dropped frame are gone; the next complete
               --  frame starts from the last committed state.
               A.Discarding := False;
               A.Building := A.Committed;
               A.Changed := False;
               Result := Frame_Unchanged;
            elsif A.Changed then
               Report := A.Building;
               A.Committed := (A.Building with delta Wheel => 0);
               A.Building := A.Committed;
               A.Changed := False;
               Result := Frame_Report;
            else
               Result := Frame_Unchanged;
            end if;
         else
            Result := Ignored;
         end if;
         return;
      end if;
      if A.Discarding then
         Result := Ignored;
         return;
      end if;
      if E.Kind = EV_ABS and then E.Code = ABS_X then
         A.Building.X := Normalize (Signed (E.Value), A.X_Range);
         A.Changed := A.Changed or else A.Building.X /= A.Committed.X;
      elsif E.Kind = EV_ABS and then E.Code = ABS_Y then
         A.Building.Y := Normalize (Signed (E.Value), A.Y_Range);
         A.Changed := A.Changed or else A.Building.Y /= A.Committed.Y;
      elsif E.Kind = EV_KEY and then E.Code in BTN_LEFT .. BTN_EXTRA then
         declare
            Bit : constant Button_State := Button_Masks (Button_Of (E.Code));
         begin
            if E.Value = KEY_RELEASED then
               A.Building.Buttons := A.Building.Buttons and not Bit;
            else
               A.Building.Buttons := A.Building.Buttons or Bit;
            end if;
            A.Changed := A.Changed or else
              A.Building.Buttons /= A.Committed.Buttons;
         end;
      elsif E.Kind = EV_REL and then E.Code = REL_WHEEL then
         --  Saturate: a frame never carries more steps than the wire holds.
         declare
            Sum : constant Long_Long_Integer :=
              Long_Long_Integer (A.Building.Wheel) + Signed (E.Value);
         begin
            A.Building.Wheel :=
              (if Sum < Long_Long_Integer (Wheel_Steps'First) then Wheel_Steps'First
               elsif Sum > Long_Long_Integer (Wheel_Steps'Last) then Wheel_Steps'Last
               else Wheel_Steps (Sum));
            A.Changed := A.Changed or else A.Building.Wheel /= 0;
         end;
      else
         Result := Ignored;
      end if;
   end Feed;
end Virtio_Input_Events;
