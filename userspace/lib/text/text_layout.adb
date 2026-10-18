with Text_UTF8;

package body Text_Layout with SPARK_Mode is
   function Advance (Text : String; Index : Positive; Column : Column_Count; Tab : Tab_Width) return Column_Count is
      Code : Text_UTF8.Code_Point;
      Cells : Text_UTF8.Cell_Width;
      Valid : Boolean;
   begin
      if Text (Index) = ASCII.HT then
         return Tab - Column mod Tab;
      end if;
      pragma Assert (Tab <= MAXIMUM_TAB_WIDTH);
      Text_UTF8.Cluster_Face (Text, Index, Code, Cells, Valid);
      return Cells;
   end Advance;

   function Next_Boundary (Text : String; Index : Positive) return Positive is
     (Index + Text_UTF8.Cluster_Size (Text, Index));

   function Column_Of (Text : String; Upto : Positive; Tab : Tab_Width) return Column_Count is
      Index : Positive := Text'First;
      Column : Column_Count := 0;
   begin
      while Index < Upto and then Index <= Text'Last loop
         pragma Loop_Invariant (Index in Text'Range and then Column <= MAXIMUM_TAB_WIDTH * (Index - Text'First));
         pragma Loop_Variant (Increases => Index);
         Column := Column + Advance (Text, Index, Column, Tab);
         Index := Next_Boundary (Text, Index);
      end loop;
      return Column;
   end Column_Of;

   function Index_At_Column
     (Text : String; Column : Column_Count; Tab : Tab_Width; Nearest : Boolean := False) return Positive
   is
      Index : Positive := Text'First;
      At_Column : Column_Count := 0;
   begin
      while Index <= Text'Last loop
         pragma Loop_Invariant (Index in Text'Range and then At_Column <= MAXIMUM_TAB_WIDTH * (Index - Text'First));
         pragma Loop_Variant (Increases => Index);
         declare
            Cells : constant Column_Count := Advance (Text, Index, At_Column, Tab);
            Next : constant Positive := Next_Boundary (Text, Index);
         begin
            if Column < At_Column + Cells then
               return (if Nearest and then Column - At_Column >= (Cells + 1) / 2 then Next else Index);
            end if;
            At_Column := At_Column + Cells;
            Index := Next;
         end;
      end loop;
      return Text'Last + 1;
   end Index_At_Column;

   function Previous_Boundary (Text : String; Index : Positive) return Positive is
      Here : Positive := Text'First;
   begin
      loop
         pragma Loop_Invariant (Here in Text'First .. Index - 1);
         pragma Loop_Variant (Increases => Here);
         declare
            Next : constant Positive := Next_Boundary (Text, Here);
         begin
            exit when Next >= Index;
            Here := Next;
         end;
      end loop;
      return Here;
   end Previous_Boundary;

   procedure Wrap
     (Text : String; Width : Column_Count; Tab : Tab_Width; Starts : out Row_Starts; Count : out Row_Index)
   is
      Row_Start : Positive := Text'First;
   begin
      Starts := [others => 1];
      Count := 1;
      if Width = 0 or else Text'Length = 0 then
         return;
      end if;
      while Row_Start <= Text'Last and then Count < MAXIMUM_ROWS loop
         pragma Loop_Invariant (Row_Start in Text'Range and then Count <= Row_Start - Text'First + 1
                                and then Starts (1) = 1
                                and then Starts (Count) = Row_Start - Text'First + 1
                                and then (for all K in 2 .. Count => Starts (K) > Starts (K - 1)
                                          and then Starts (K) <= Text'Length));
         pragma Loop_Variant (Increases => Row_Start);
         declare
            Index : Positive := Row_Start;
            Column : Column_Count := 0;
            After_Space : Natural := 0;   --  the boundary after the last space
            Break : Natural := 0;
         begin
            while Index <= Text'Last loop
               pragma Loop_Invariant (Index in Row_Start .. Text'Last
                                      and then Column <= MAXIMUM_TAB_WIDTH * (Index - Row_Start)
                                      and then After_Space in 0 | Row_Start + 1 .. Index);
               pragma Loop_Variant (Increases => Index);
               declare
                  Cells : constant Column_Count := Advance (Text, Index, Column, Tab);
                  Next : constant Positive := Next_Boundary (Text, Index);
               begin
                  if Column + Cells > Width and then Index > Row_Start then
                     Break := (if After_Space > Row_Start then After_Space else Index);
                     exit;
                  end if;
                  Column := Column + Cells;
                  if Text (Index) = ' ' or else Text (Index) = ASCII.HT then
                     After_Space := Next;
                  end if;
                  exit when Next > Text'Last;
                  Index := Next;
               end;
            end loop;
            exit when Break = 0 or else Break > Text'Last;
            Count := Count + 1;
            Starts (Count) := Break - Text'First + 1;
            Row_Start := Break;
         end;
      end loop;
   end Wrap;
end Text_Layout;
