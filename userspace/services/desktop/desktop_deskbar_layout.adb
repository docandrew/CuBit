------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's arithmetic (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Deskbar_Layout with SPARK_Mode is

   function Widths
     (Left, Right : Coordinate; Preferred : Width_List;
      Minimum : Button_Width_Range; Gap : Gap_Range; Active : Button_Count) return Width_List
   is
      --  Sums over every button reach Max_Buttons * (Max_Button_Width +
      --  Max_Gap), and the proportional share multiplies two of them.
      type Wide is range 0 .. 2 ** 62;
      Room : constant Natural := Right - Left;
      Total : Wide := 0;          --  every Preferred width and the gaps
      Slack : Wide := 0;          --  how far the inactive ones are above Minimum
      Result : Width_List := Preferred;
   begin
      for P in Preferred'Range loop
         Total := Total + Wide (Preferred (P)) + (if P > 1 then Wide (Gap) else 0);
         if P /= Active then
            Slack := Slack + Wide (Preferred (P) - Minimum);
         end if;
         pragma Loop_Invariant (Total <= Wide (P) * (Max_Button_Width + Max_Gap));
         pragma Loop_Invariant (Slack <= Wide (P) * Max_Button_Width);
      end loop;
      if Total <= Wide (Room) then
         return Result;
      end if;
      declare
         Excess : constant Wide := Total - Wide (Room);
      begin
         if Slack >= Excess then
            --  The inactive buttons give up Excess between them, each in
            --  proportion to its own slack: their sum then fits exactly.
            for P in Preferred'Range loop
               if P /= Active then
                  Result (P) := Natural'Min
                    (Preferred (P),
                     Minimum + Natural (Wide'Min (Wide (Preferred (P) - Minimum),
                                                  Wide (Preferred (P) - Minimum) * (Slack - Excess) / Slack)));
               end if;
               pragma Loop_Invariant
                 (for all K in Preferred'Range => Result (K) >= Minimum and then Result (K) <= Preferred (K));
            end loop;
         else
            --  Every inactive button at Minimum, the active one gives up the rest.
            for P in Preferred'Range loop
               if P /= Active then
                  Result (P) := Minimum;
               end if;
               pragma Loop_Invariant
                 (for all K in Preferred'Range => Result (K) >= Minimum and then Result (K) <= Preferred (K));
            end loop;
            if Active in Preferred'Range then
               Result (Active) := Natural'Max
                 (Minimum, Preferred (Active) -
                    Natural (Wide'Min (Wide (Preferred (Active) - Minimum), Excess - Slack)));
            end if;
         end if;
      end;
      return Result;
   end Widths;

   function Spans
     (Left, Right : Coordinate; Preferred : Width_List;
      Minimum : Button_Width_Range; Gap : Gap_Range; Active : Button_Count) return Span_List
   is
      W : constant Width_List := Widths (Left, Right, Preferred, Minimum, Gap, Active);
      Cursor : Coordinate := Left;   --  where the next button may start
      Result : Span_List (Preferred'Range) := [others => (X => Left, W => 0)];
   begin
      for P in Preferred'Range loop
         exit when W (P) > Right - Cursor;
         Result (P) := (X => Cursor, W => W (P));
         Cursor := Cursor + W (P);
         Cursor := (if Gap <= Right - Cursor then Cursor + Gap else Right);
         pragma Loop_Invariant (Left <= Cursor and then Cursor <= Right);
         pragma Loop_Invariant
           (for all K in 1 .. P =>
              (if Result (K).W /= 0 then
                 Result (K).X >= Left and then Result (K).X + Result (K).W <= Cursor
                 and then Result (K).W >= Minimum and then Result (K).W <= Preferred (K)));
         pragma Loop_Invariant
           (for all K in 1 .. P =>
              (for all Q in K + 1 .. P =>
                 (if Result (K).W /= 0 and then Result (Q).W /= 0 then
                    Result (K).X + Result (K).W <= Result (Q).X)));
         pragma Loop_Invariant (for all K in P + 1 .. Preferred'Last => Result (K).W = 0);
      end loop;
      return Result;
   end Spans;

   function Level_At (Track_X : Coordinate; Track_W : Track_Width; X : Coordinate)
     return Percent is
   begin
      if X <= Track_X then
         return 0;
      elsif X - Track_X >= Track_W then
         return 100;
      end if;
      return ((X - Track_X) * 100 + Track_W / 2) / Track_W;
   end Level_At;

   function Offset_Of (Track_W : Track_Width; Level : Percent) return Natural is
     (Track_W * Level / 100);

   --  Sakamoto's method: the offset of each month's first day, in weekdays.
   type Month_Offsets is array (Month_Number) of Natural range 0 .. 6;
   Offsets : constant Month_Offsets := [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4];

   function Day_Of_Week (Year : Year_Number; Month : Month_Number; Day : Day_Number)
     return Weekday
   is
      Y : constant Natural := (if Month < 3 then Year - 1 else Year);
   begin
      return Weekday'Val ((Y + Y / 4 - Y / 100 + Y / 400 + Offsets (Month) + Day) mod 7);
   end Day_Of_Week;

   function Weekday_Name (Day : Weekday) return String is
     (case Day is
         when Sunday    => "Sunday",
         when Monday    => "Monday",
         when Tuesday   => "Tuesday",
         when Wednesday => "Wednesday",
         when Thursday  => "Thursday",
         when Friday    => "Friday",
         when Saturday  => "Saturday");

   function Month_Name (Month : Month_Number) return String is
     (case Month is
         when 1  => "January",
         when 2  => "February",
         when 3  => "March",
         when 4  => "April",
         when 5  => "May",
         when 6  => "June",
         when 7  => "July",
         when 8  => "August",
         when 9  => "September",
         when 10 => "October",
         when 11 => "November",
         when 12 => "December");

end Desktop_Deskbar_Layout;
