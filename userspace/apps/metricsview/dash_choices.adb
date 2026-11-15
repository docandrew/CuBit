pragma Ada_2022;
with Charts_Buckets;

package body Dash_Choices is
   use Dash_Model;

   Title_Separator : constant String := ", ";
   Prefix_Mark : constant String := "*";

   function Caption (Choice : View_Choice) return String is
     (case Choice is
        when Lines_Median_And_Tail => "Lines: p50 and p99, min to max band",
        when Line_Tail => "Line: p99",
        when Line_Mean => "Line: mean",
        when Line_Maximum => "Line: maximum",
        when Line_Rate => "Line: rate per second",
        when Heatmap_Tail => "Heatmap: p99 per series",
        when Heatmap_Median => "Heatmap: p50 per series",
        when Histogram_Of_Values => "Histogram of values",
        when Number_Tail => "Number: p99",
        when Number_Median => "Number: p50",
        when Number_Last => "Number: latest value",
        when Number_Count => "Number: count",
        when Number_Rate => "Number: rate per second");

   function Short_Name (Choice : View_Choice) return String is
     (case Choice is
        when Lines_Median_And_Tail => "p50 and p99",
        when Line_Tail | Heatmap_Tail | Number_Tail => "p99",
        when Heatmap_Median | Number_Median => "p50",
        when Line_Mean => "mean",
        when Line_Maximum => "max",
        when Line_Rate | Number_Rate => "per second",
        when Histogram_Of_Values => "histogram",
        when Number_Last => "latest",
        when Number_Count => "count");

   --  A line view's lines and band.
   type Line_Shape is record
      Lines : Statistic_Set := [others => False];
      Band : Boolean := False;
   end record;
   function Shape_Of (Choice : View_Choice) return Line_Shape is
     (case Choice is
        when Lines_Median_And_Tail => ([P50 | P99 => True, others => False], True),
        when Line_Tail => ([P99 => True, others => False], False),
        when Line_Mean => ([Mean => True, others => False], False),
        when Line_Maximum => ([Maximum => True, others => False], False),
        when Line_Rate => ([Rate => True, others => False], False),
        when others => ([others => False], False));

   --  The statistic a heatmap or a number shows.
   function Shown_Of (Choice : View_Choice) return Statistic is
     (case Choice is
        when Heatmap_Median | Number_Median => P50,
        when Number_Last => Last,
        when Number_Count => Count,
        when Number_Rate => Rate,
        when others => P99);

   function Kind_Of (Choice : View_Choice) return Panel_Kind is
     (case Choice is
        when Lines_Median_And_Tail .. Line_Rate => Time_Series,
        when Heatmap_Tail | Heatmap_Median => Heatmap,
        when Histogram_Of_Values => Histogram,
        when Number_Tail .. Number_Rate => Stat);

   --  Counts and rates are not in the series' unit: no budget.
   function In_Series_Unit (Choice : View_Choice) return Boolean is
     (Choice not in Line_Rate | Number_Count | Number_Rate);

   procedure View_Of (P : Panel; Choice : out View_Choice; Found : out Boolean) is
   begin
      Choice := View_Choice'First;
      Found := False;
      for C in View_Choice loop
         if Kind_Of (C) = P.Kind then
            case P.Kind is
               when Time_Series =>
                  Found := Shape_Of (C).Lines = P.Lines and then Shape_Of (C).Band = P.Band;
               when Heatmap | Stat =>
                  Found := Shown_Of (C) = P.Shown;
               when Histogram =>
                  Found := True;
               when Logs =>
                  Found := False;
            end case;
            if Found then
               Choice := C;
               return;
            end if;
         end if;
      end loop;
   end View_Of;

   function Ends_With (S, Suffix : String) return Boolean is
     (S'Length >= Suffix'Length and then S (S'Last - Suffix'Length + 1 .. S'Last) = Suffix);

   function With_View (P : Panel; Choice : View_Choice; Value_Top : Unsigned_64) return Panel is
      Result : Panel := P;
      Old : View_Choice;
      Had : Boolean;
      Title : constant String := Image (P.Title);
   begin
      View_Of (P, Old, Had);
      Result.Kind := Kind_Of (Choice);
      case Result.Kind is
         when Time_Series =>
            Result.Lines := Shape_Of (Choice).Lines;
            Result.Band := Shape_Of (Choice).Band;
         when Heatmap | Stat =>
            Result.Shown := Shown_Of (Choice);
         when Histogram =>
            if P.Kind /= Histogram then
               declare
                  Top : constant Unsigned_64 :=
                    Unsigned_64'Min (Unsigned_64'Max (Value_Top, Histogram_Buckets), Charts_Buckets.Maximum_Bucket_Value);
               begin
                  Result.Buckets :=
                    (First => 0, Width => (Top + Histogram_Buckets - 1) / Histogram_Buckets, Count => Histogram_Buckets);
               end;
            end if;
         when Logs =>
            null;
      end case;
      if not In_Series_Unit (Choice) or else (Had and then not In_Series_Unit (Old)) then
         Result.Budget := 0;
      end if;
      --  The title: a picked one follows; one that names its view by the
      --  old view's name takes the new one's.
      if Title = New_Title or else (Had and then Title = Title_For (P)) then
         Result.Title := To_Text (Title_For (Result));
      elsif Had and then Ends_With (Title, Title_Separator & Short_Name (Old)) then
         Result.Title := To_Text (Title (Title'First .. Title'Last - Short_Name (Old)'Length) & Short_Name (Choice));
      end if;
      return Result;
   end With_View;

   function Caption (Source : Source_Choice) return String is
     (Image (Source.Name) & (if Source.Match = Prefix then Prefix_Mark else ""));

   procedure Source_Of (P : Panel; Source : out Source_Choice; Found : out Boolean) is
   begin
      Found := P.Series_Used = 1 and then P.Series (1).Source.Length = 0;
      Source := (if Found then (P.Series (1).Name, P.Series (1).Match) else (others => <>));
   end Source_Of;

   function With_Source (P : Panel; Source : Source_Choice) return Panel is
      Result : Panel := P;
   begin
      Result.Series_Used := 1;
      Result.Series (1) := (Name => Source.Name, Source => (others => <>), Label => (others => <>),
                            Match => Source.Match);
      --  The series is new: so is what the title says.
      Result.Title := To_Text (Title_For (Result));
      return Result;
   end With_Source;

   function Title_For (P : Panel) return String is
      Choice : View_Choice;
      Found : Boolean;
      Source : Source_Choice;
      One : Boolean;
   begin
      View_Of (P, Choice, Found);
      Source_Of (P, Source, One);
      if not One then
         return New_Title;
      end if;
      return Caption (Source) & (if Found then Title_Separator & Short_Name (Choice) else "");
   end Title_For;

   function New_Panel return Panel is
      Result : Panel;
   begin
      Result.Title := To_Text (New_Title);
      Result.Kind := Time_Series;
      Result.Lines := Shape_Of (Lines_Median_And_Tail).Lines;
      Result.Band := Shape_Of (Lines_Median_And_Tail).Band;
      Result.Series_Used := 0;
      Result.Width := New_Width;
      Result.Height := New_Height;
      return Result;
   end New_Panel;

   function New_Dashboard (Title : String) return Dashboard is
      Result : Dashboard;
   begin
      Result.Title := To_Text (Title);
      Result.Span := New_Span_Ms;
      Result.Live := True;
      Result.Panels (1) := New_Panel;
      Result.Panels (1).Id := 1;
      Result.Panels_Used := 1;
      return Result;
   end New_Dashboard;

   function Group_Of (Name : String) return String is
   begin
      for K in reverse Name'Range loop
         if Name (K) = '.' then
            return Name (Name'First .. K);
         end if;
      end loop;
      return "";
   end Group_Of;
end Dash_Choices;
