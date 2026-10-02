with Servo_Tabs; use Servo_Tabs;
with Servo_Tab_Geometry;
with Ada.Text_IO;
procedure Tabs_Tests is
   S : State;
   Added : Selection;
   P : Servo_Tab_Geometry.Rectangle;
begin
   for Round in 1 .. 1000 loop
      S := (others => <>);
      for I in 2 .. Capacity loop
         Open_Tab (S, Added);
         pragma Assert (Added = I and Valid (S) and Count (S) = I);
      end loop;
      Open_Tab (S, Added); pragma Assert (Added = 0);
      Close_Tab (S, 3); Open_Tab (S, Added); pragma Assert (Added = 0);
      Parked (S, 4); Open_Tab (S, Added); pragma Assert (Added = 0);
      Parked (S, 3); Open_Tab (S, Added); pragma Assert (Added = 3);
      Cycle (S, True); pragma Assert (S.Active = 2);
      Cycle (S, False); pragma Assert (S.Active = 3);
      for I in Slot loop Close_Tab (S, I); pragma Assert (Valid (S)); end loop;
      pragma Assert (S.Active = 0 and Count (S) = 0);
   end loop;
   for W in 0 .. 1024 loop
      for Vertical in Boolean loop
         P := Servo_Tab_Geometry.Page (W, 600, Vertical);
         pragma Assert (P.X + P.W = W and P.Y + P.H = 576);
      end loop;
   end loop;
   for Width in Servo_Tab_Geometry.Window_Width loop
      for Count in Servo_Tab_Geometry.Tab_Count loop
         for Rank in 0 .. Servo_Tab_Geometry.Visible (Width, 600, False, Count) - 1 loop
            P := Servo_Tab_Geometry.Tab (Width, Rank, False,
              Servo_Tab_Geometry.Visible (Width, 600, False, Count));
            pragma Assert (P.X + P.W <= Width and P.W >= 32 and P.W <= 220);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Servo tabs PASS: 1000 capacity/parking/reuse/selection cycles and geometry");
end Tabs_Tests;
