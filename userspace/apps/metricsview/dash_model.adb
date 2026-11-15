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
end Dash_Model;
