pragma Ada_2022;
package body Host_Bridge_Rows with SPARK_Mode is

   function Window_Span (R : W.Row) return PL.Span is
      Low  : constant W.Word := R (W.Minimum);
      High : constant W.Word :=
        (if Has (R, W.Has_Length) and then R (W.Length) /= 0
           and then R (W.Length) - 1 <= R (W.Maximum) - Low
         then Low + (R (W.Length) - 1) else R (W.Maximum));
      First_Page : constant W.Word := Low / Page_Bytes + (if Low mod Page_Bytes = 0 then 0 else 1);
      --  Pages wholly at or below High.
      Past_Page  : constant W.Word := (if High = W.Word'Last then High / Page_Bytes + 1
                                       else (High + 1) / Page_Bytes);
   begin
      if First_Page < Past_Page and then Past_Page <= PL.Page_Limit then
         return (First => First_Page, Past => Past_Page);
      end if;
      return (0, 0);
   end Window_Span;

   procedure Windows
     (Items : W.Rows; Spans : out PL.Span_Array; Prefetch : out Flags;
      Count : out Natural)
   is
   begin
      Spans := [others => (0, 0)];
      Prefetch := [others => False];
      Count := 0;
      for I in Items'Range loop
         pragma Loop_Invariant (Count <= I - Items'First);
         pragma Loop_Invariant (for all K in 1 .. Count => PL.Valid (Spans (K)));
         if Is_Window (Items (I)) then
            declare
               S : constant PL.Span := Window_Span (Items (I));
            begin
               if PL.Valid (S) then
                  Count := Count + 1;
                  Spans (Count) := S;
                  Prefetch (Count) := Prefetchable (Items (I));
               end if;
            end;
         end if;
      end loop;
   end Windows;

   procedure Buses
     (Items : W.Rows; Base_Bus_Present : Boolean; Base_Bus : W.Word;
      First, Last : out Unsigned_8)
   is
   begin
      First := (if Base_Bus_Present and then Base_Bus <= Last_Bus_Number then Unsigned_8 (Base_Bus) else 0);
      Last := Last_Bus_Number;
      for R of Items loop
         if Is_Address_Form (R (W.Form)) and then R (W.Kind) = W.Bus_Kind and then Producer (R)
           and then Has (R, W.Has_Minimum) and then Has (R, W.Has_Maximum)
           and then R (W.Minimum) <= R (W.Maximum) and then R (W.Maximum) <= Last_Bus_Number
         then
            First := Unsigned_8 (R (W.Minimum));
            Last := Unsigned_8 (R (W.Maximum));
            return;
         end if;
      end loop;
   end Buses;
end Host_Bridge_Rows;
