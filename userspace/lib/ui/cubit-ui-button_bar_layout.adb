------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Button_Bar_Layout with SPARK_Mode is

   --  The width of the cluster starting at First: its buttons and the
   --  gaps between them, saturating at MAXIMUM_COORDINATE + 1 (too wide).
   function Cluster_Width
     (Widths : Width_Table; Starts : Start_Table; Count : Button_Count; First : Button_Index;
      Gap : Coordinate) return Natural
   with Pre => First <= Count, Post => Cluster_Width'Result <= MAXIMUM_COORDINATE + 1
   is
      Total : Natural := Widths (First);
   begin
      for K in First + 1 .. Count loop
         exit when Starts (K);
         if Total + Gap + Widths (K) > MAXIMUM_COORDINATE then
            return MAXIMUM_COORDINATE + 1;
         end if;
         Total := Total + Gap + Widths (K);
         pragma Loop_Invariant (Total <= MAXIMUM_COORDINATE);
      end loop;
      return Total;
   end Cluster_Width;

   procedure Arrange
     (Widths : Width_Table; Starts : Start_Table; Count : Button_Count;
      Available, Gap, Cluster_Gap : Coordinate;
      Places : out Place_Table; Rows : out Row_Number)
   is
      Row : Row_Number := 0;
      --  The pen: where the row's last button ended.
      Pen : Natural := 0;
   begin
      Places := [others => (Row => 0, X => 0)];
      Rows := 0;
      if Count = 0 then
         return;
      end if;
      for K in 1 .. Count loop
         declare
            Lead : constant Natural := (if K = 1 then 0 elsif Starts (K) then Natural'Max (Gap, Cluster_Gap) else Gap);
            Wrap : Boolean :=
              K > 1 and then Pen + Lead + Widths (K) > Available;
         begin
            --  A cluster starts on a fresh row when it doesn't fit whole
            --  after the one before (F5..F8 move down together).
            if K > 1 and then Starts (K) and then not Wrap
              and then Pen + Lead + Cluster_Width (Widths, Starts, Count, K, Gap) > Available
            then
               Wrap := True;
            end if;
            if Wrap then
               Row := Row + 1;
               Places (K) := (Row => Row, X => 0);
               Pen := Widths (K);
            else
               Places (K) := (Row => Row, X => Pen + Lead);
               Pen := Pen + Lead + Widths (K);
            end if;
         end;
         pragma Loop_Invariant (Row <= K - 1);
         pragma Loop_Invariant (Pen = Ends (Places (K), Widths (K)));
         pragma Loop_Invariant (Places (K).Row = Row);
         pragma Loop_Invariant (Pen <= Available or else Places (K).X = 0);
         pragma Loop_Invariant (Places (1) = (Row => 0, X => 0));
         pragma Loop_Invariant
           (for all J in 1 .. K =>
              Places (J).Row <= Row
              and then (Ends (Places (J), Widths (J)) <= Available or else Places (J).X = 0));
         pragma Loop_Invariant
           (for all J in 2 .. K =>
              Places (J).Row in Places (J - 1).Row .. Places (J - 1).Row + 1
              and then (if Places (J).Row = Places (J - 1).Row then
                          Places (J).X >= Ends (Places (J - 1), Widths (J - 1)) + Gap
                        else Places (J).X = 0));
      end loop;
      Rows := Row + 1;
   end Arrange;

end CuBit.UI.Button_Bar_Layout;
