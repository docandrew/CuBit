pragma Ada_2022;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Typed_Values;

package body Dash_Model is
   package Views renames CCL.Objects.Views;

   --  Fields by declaration order (TYPE_SOURCE).
   Dashboard_Title : constant := 1;
   Dashboard_Time_Field : constant := 2;
   Dashboard_Panels : constant := 3;
   Time_Span : constant := 1;
   Time_Live : constant := 2;
   Panel_Title : constant := 1;
   Panel_Kind_Field : constant := 2;
   Panel_Series_Field : constant := 3;
   Panel_Width : constant := 4;
   Panel_Height : constant := 5;
   Series_Name : constant := 1;
   Series_Source : constant := 2;
   Series_Match_Field : constant := 3;
   Series_Label : constant := 4;
   Line_Statistics : constant := 1;
   Line_Band : constant := 2;
   Line_Budget : constant := 3;
   Histogram_Buckets : constant := 1;
   Histogram_Budget : constant := 2;
   Shown_Statistic : constant := 1;
   Shown_Budget : constant := 2;
   Buckets_First : constant := 1;
   Buckets_Width : constant := 2;
   Buckets_Count : constant := 3;
   Logs_Service_Field : constant := 1;
   Logs_Minimum_Field : constant := 2;

   --  The checker's value and the dashboard being read: large, kept here
   --  rather than on the stack.
   Object : Views.Snapshot;
   D : Dashboard;

   function To_Text (S : String) return Text is
      Kept : constant Text_Length := Natural'Min (S'Length, Maximum_Text);
      Result : Text;
   begin
      Result.Length := Kept;
      Result.Value (1 .. Kept) := S (S'First .. S'First + Kept - 1);
      return Result;
   end To_Text;

   function Text_At (At_Cursor : Views.Cursor) return Text is (To_Text (Views.Text (Object, At_Cursor)));

   function Number_At (At_Cursor : Views.Cursor) return Unsigned_64 is
      Value : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, At_Cursor));
   begin
      return (if Value < 0 then 0 else Unsigned_64 (Value));
   end Number_At;

   function Flag_At (At_Cursor : Views.Cursor) return Boolean is (Views.Scalar (Object, At_Cursor).First = 1);

   --  An enum's member, 1-based in declaration order.
   function Member_At (At_Cursor : Views.Cursor) return Natural is (Views.Alternative (Object, At_Cursor));

   function Statistic_At (At_Cursor : Views.Cursor) return Statistic is
      Choice : constant Natural := Member_At (At_Cursor);
   begin
      return (if Choice in 1 .. Statistic'Pos (Statistic'Last) + 1 then Statistic'Val (Choice - 1) else P99);
   end Statistic_At;

   procedure Read_Series (At_Cursor : Views.Cursor; Into : out Series_Spec);
   procedure Read_Series (At_Cursor : Views.Cursor; Into : out Series_Spec) is
   begin
      Into.Name := Text_At (Views.Field (Object, At_Cursor, Series_Name));
      Into.Source := Text_At (Views.Field (Object, At_Cursor, Series_Source));
      Into.Label := Text_At (Views.Field (Object, At_Cursor, Series_Label));
      Into.Match := (if Member_At (Views.Field (Object, At_Cursor, Series_Match_Field)) = 2 then Prefix else Exact);
   end Read_Series;

   procedure Read_Kind (At_Cursor : Views.Cursor; Into : in out Panel);
   procedure Read_Kind (At_Cursor : Views.Cursor; Into : in out Panel) is
      Choice : constant Natural := Member_At (At_Cursor);
      Body_Cursor : constant Views.Cursor := Views.Payload (Object, At_Cursor);
   begin
      Into.Kind := (if Choice in 1 .. Panel_Kind'Pos (Panel_Kind'Last) + 1 then Panel_Kind'Val (Choice - 1)
                    else Time_Series);
      case Into.Kind is
         when Time_Series =>
            declare
               Listed : constant Views.Cursor := Views.Field (Object, Body_Cursor, Line_Statistics);
            begin
               Into.Lines := [others => False];
               for I in 1 .. Views.Length (Object, Listed) loop
                  Into.Lines (Statistic_At (Views.Element (Object, Listed, I))) := True;
               end loop;
               Into.Band := Flag_At (Views.Field (Object, Body_Cursor, Line_Band));
               Into.Budget := Number_At (Views.Field (Object, Body_Cursor, Line_Budget));
            end;
         when Histogram =>
            declare
               B : constant Views.Cursor := Views.Field (Object, Body_Cursor, Histogram_Buckets);
            begin
               Into.Buckets :=
                 (First => Number_At (Views.Field (Object, B, Buckets_First)),
                  Width => Number_At (Views.Field (Object, B, Buckets_Width)),
                  Count => Natural (Number_At (Views.Field (Object, B, Buckets_Count))));
               Into.Budget := Number_At (Views.Field (Object, Body_Cursor, Histogram_Budget));
            end;
         when Heatmap | Stat =>
            Into.Shown := Statistic_At (Views.Field (Object, Body_Cursor, Shown_Statistic));
            Into.Budget := Number_At (Views.Field (Object, Body_Cursor, Shown_Budget));
         when Logs =>
            Into.Logs_Service := Text_At (Views.Field (Object, Body_Cursor, Logs_Service_Field));
            Into.Logs_Minimum := Natural (Unsigned_64'Min (Number_At (Views.Field (Object, Body_Cursor,
                                                                                    Logs_Minimum_Field)),
                                                           Unsigned_64 (Natural'Last)));
      end case;
   end Read_Kind;

   procedure Fail (Result : out Load_Result; Why : String; Position : Natural);
   procedure Fail (Result : out Load_Result; Why : String; Position : Natural) is
      Kept : constant Natural := Natural'Min (Why'Length, Maximum_Message);
   begin
      Result := (others => <>);
      Result.Message (1 .. Kept) := Why (Why'First .. Why'First + Kept - 1);
      Result.Message_Length := Kept;
      Result.Position := Position;
   end Fail;

   procedure Load (Source : String; Into : in out Dashboard; Result : out Load_Result) is
      Checked : CCL.Typed_Values.Check_Result;
   begin
      D := (others => <>);
      CCL.Typed_Values.Capture_Value (TYPE_SOURCE, ROOT_NAME, Source, Object, Checked);
      if not Checked.Success then
         Fail (Result, Checked.Message (1 .. Checked.Message_Length), Checked.Position);
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         Time : constant Views.Cursor := Views.Field (Object, Root, Dashboard_Time_Field);
         Panels : constant Views.Cursor := Views.Field (Object, Root, Dashboard_Panels);
      begin
         D.Title := Text_At (Views.Field (Object, Root, Dashboard_Title));
         D.Span := Number_At (Views.Field (Object, Time, Time_Span));
         D.Live := Flag_At (Views.Field (Object, Time, Time_Live));
         if Views.Length (Object, Panels) > Maximum_Panels then
            Fail (Result, "more panels than a dashboard holds", 0);
            return;
         end if;
         for P in 1 .. Views.Length (Object, Panels) loop
            declare
               At_Panel : constant Views.Cursor := Views.Element (Object, Panels, P);
               Listed : constant Views.Cursor := Views.Field (Object, At_Panel, Panel_Series_Field);
               Item : Panel renames D.Panels (P);
            begin
               Item.Id := P;
               Item.Title := Text_At (Views.Field (Object, At_Panel, Panel_Title));
               Item.Width := Natural (Number_At (Views.Field (Object, At_Panel, Panel_Width)));
               Item.Height := Natural (Number_At (Views.Field (Object, At_Panel, Panel_Height)));
               Read_Kind (Views.Field (Object, At_Panel, Panel_Kind_Field), Item);
               if Views.Length (Object, Listed) > Maximum_Panel_Series then
                  Fail (Result, "more series in a panel than it holds", 0);
                  return;
               end if;
               for S in 1 .. Views.Length (Object, Listed) loop
                  Read_Series (Views.Element (Object, Listed, S), Item.Series (S));
               end loop;
               Item.Series_Used := Views.Length (Object, Listed);
            end;
         end loop;
         D.Panels_Used := Views.Length (Object, Panels);
      end;
      Into := D;
      Result := (Success => True, others => <>);
   end Load;
   ---------------------------------------------------------------------------
   --  Source
   ---------------------------------------------------------------------------
   function Decimal (V : Unsigned_64) return String is
     (if V < 10 then [Character'Val (Character'Pos ('0') + Natural (V))]
      else Decimal (V / 10) & [Character'Val (Character'Pos ('0') + Natural (V mod 10))]);

   --  A CCL string literal: quotes and backslashes escaped.
   function Escaped (S : String) return String is
   begin
      for K in S'Range loop
         if S (K) in '"' | '\' then
            return S (S'First .. K - 1) & '\' & S (K) & Escaped (S (K + 1 .. S'Last));
         end if;
      end loop;
      return S;
   end Escaped;
   function Quoted (S : String) return String is ('"' & Escaped (S) & '"');

   function Statistic_Source (S : Statistic) return String is
     ("Metric_Statistic." & (case S is
        when Minimum => "Minimum", when P50 => "P50", when P99 => "P99", when Maximum => "Maximum",
        when Mean => "Mean", when Count => "Count", when Rate => "Rate", when Last => "Last"));

   function Spec_Source (S : Series_Spec) return String is
     (if S.Source.Length = 0 and then S.Match = Exact then
        "(series " & Quoted (Image (S.Name)) & " " & Quoted (Image (S.Label)) & ")"
      elsif S.Source.Length = 0 then
        "(prefix " & Quoted (Image (S.Name)) & " " & Quoted (Image (S.Label)) & ")"
      else
        "(Panel_Series name => " & Quoted (Image (S.Name)) & " source => " & Quoted (Image (S.Source))
        & " match => Series_Match." & (if S.Match = Exact then "Exact" else "Prefix")
        & " label => " & Quoted (Image (S.Label)) & ")");

   function Panel_Source (P : Panel) return String is
      function Lines_Source return String is
         function From (S : Statistic) return String is
           ((if P.Lines (S) then " " & Statistic_Source (S) else "")
            & (if S = Statistic'Last then "" else From (Statistic'Succ (S))));
         All_Lines : constant String := From (Statistic'First);
      begin
         return "[" & (if All_Lines'Length > 0 then All_Lines (All_Lines'First + 1 .. All_Lines'Last) else "") & "]";
      end Lines_Source;
      function Series_List_Source (From : Positive) return String is
        (if From > P.Series_Used then ""
         else (if From > 1 then " " else "") & Spec_Source (P.Series (From)) & Series_List_Source (From + 1));
      Kind : constant String :=
        (case P.Kind is
           when Time_Series =>
             "(Panel_Kind.Time_Series (Line_Panel statistics => " & Lines_Source & " band => "
             & (if P.Band then "true" else "false") & " budget => " & Decimal (P.Budget) & "))",
           when Histogram =>
             "(Panel_Kind.Histogram (Histogram_Panel buckets => (Linear_Buckets first => "
             & Decimal (P.Buckets.First) & " width => " & Decimal (P.Buckets.Width) & " count => "
             & Decimal (Unsigned_64 (P.Buckets.Count)) & ") budget => " & Decimal (P.Budget) & "))",
           when Heatmap =>
             "(Panel_Kind.Heatmap (Heatmap_Panel statistic => " & Statistic_Source (P.Shown) & " budget => "
             & Decimal (P.Budget) & "))",
           when Stat =>
             "(Panel_Kind.Stat (Stat_Panel statistic => " & Statistic_Source (P.Shown) & " budget => "
             & Decimal (P.Budget) & "))",
           when Logs =>
             "(Panel_Kind.Logs (Logs_Panel service => " & Quoted (Image (P.Logs_Service)) & " minimum => "
             & Decimal (Unsigned_64 (P.Logs_Minimum)) & "))");
   begin
      return "(Panel title => " & Quoted (Image (P.Title)) & " width => " & Decimal (Unsigned_64 (P.Width))
        & " height => " & Decimal (Unsigned_64 (P.Height)) & " kind => " & Kind & " series => ["
        & Series_List_Source (1) & "])";
   end Panel_Source;

   function File_Name (D : Dashboard) return String is
      Title : constant String := Image (D.Title);
      Slug : String (1 .. Title'Length);
      Used : Natural := 0;
      Gap : Boolean := False;
      Lower_Offset : constant := Character'Pos ('a') - Character'Pos ('A');
   begin
      for C of Title loop
         if C in 'a' .. 'z' | '0' .. '9' | 'A' .. 'Z' then
            if Gap and then Used > 0 then
               Used := Used + 1;
               Slug (Used) := '-';
            end if;
            Gap := False;
            Used := Used + 1;
            Slug (Used) := (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + Lower_Offset) else C);
         else
            Gap := True;
         end if;
      end loop;
      return Dashboards_Place & (if Used = 0 then "dashboard" else Slug (1 .. Used)) & Dashboard_Suffix;
   end File_Name;

   function To_Source (D : Dashboard) return String is
      function Panels_Source (From : Positive) return String is
        (if From > D.Panels_Used then ""
         else (if From > 1 then " " else "") & Panel_Source (D.Panels (From)) & Panels_Source (From + 1));
   begin
      return "(Dashboard title => " & Quoted (Image (D.Title)) & " time => (Dashboard_Time span_ms => "
        & Decimal (D.Span) & " live => " & (if D.Live then "true" else "false") & ") panels => ["
        & Panels_Source (1) & "])";
   end To_Source;
end Dash_Model;
