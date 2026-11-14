with CCL.Interfaces.Metrics;
with CCL.Objects;
with CCL.Objects.Views;
with CCL_Metric_IO;
with CuBit.Failures;
with CuBit.Metric_Query_Pages;

package body CCL_Metric_Bindings is
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Integer_64;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   package M renames CCL.Interfaces.Metrics;
   package Views renames CCL.Objects.Views;
   package Pages renames CuBit.Metric_Query_Pages;
   package Failures renames CuBit.Failures;
   subtype U64 is Interfaces.Unsigned_64;

   --  A selector's name ending so is a prefix.
   Prefix_Mark : constant Character := '*';
   Maximum_Text : constant := Pages.Text_Bytes;

   Bound : M.Contracts;

   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (Binding in M.FIRST_BINDING .. M.Binding_Of (M.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Success := True;
      --  Without a metrics-observer endpoint, metrics is simply absent.
      if not CCL_Metric_IO.Available then return; end if;
      M.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, M.SERIES_KEY, Bound.Series_List);
      CCL.Catalog.Resolve_Schema (Catalog, M.ITEMS_KEY, Bound.Items);
      CCL.Catalog.Resolve_Schema (Catalog, M.SELECTOR_KEY, Bound.Selector);
      CCL.Catalog.Resolve_Schema (Catalog, M.RANGE_KEY, Bound.Range_Record);
      CCL.Catalog.Resolve_Schema (Catalog, M.WINDOW_KEY, Bound.Window_Record);
      CCL.Catalog.Resolve_Schema (Catalog, M.STATS_KEY, Bound.Stats);
      Success := CCL.Objects.Is_Bound (Bound.Series_List) and then CCL.Objects.Is_Bound (Bound.Items)
        and then CCL.Objects.Is_Bound (Bound.Selector) and then CCL.Objects.Is_Bound (Bound.Range_Record)
        and then CCL.Objects.Is_Bound (Bound.Window_Record) and then CCL.Objects.Is_Bound (Bound.Stats);
      for Op in M.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "metrics." & M.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, M.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   --  A selector's name: "" every series, a trailing * a prefix, else exact.
   function Match_Of (Name : String) return CCL_Metric_IO.Name_Match is
     (if Name'Length = 0 then CCL_Metric_IO.Any_Name
      elsif Name (Name'Last) = Prefix_Mark then CCL_Metric_IO.Name_Prefix
      else CCL_Metric_IO.Exact_Name);
   function Stem (Name : String) return String is
     (if Name'Length > 0 and then Name (Name'Last) = Prefix_Mark then Name (Name'First .. Name'Last - 1)
      else Name);

   --  A from or to of 0 or less is that long before now.
   function Time_Of (Value : Interfaces.Integer_64) return U64 is
     (if Value > 0 then U64 (Value)
      elsif Value = Interfaces.Integer_64'First or else U64 (-Value) > CCL_Metric_IO.Now_Us then 0
      else CCL_Metric_IO.Now_Us - U64 (-Value));

   function Too_Long (Name, Source : String) return Boolean is
     (Name'Length > Maximum_Text or else Source'Length > Maximum_Text);
   NAME_TOO_LONG : constant Failures.Failure := Failures.Failed
     (Failures.Invalid_Argument, "a series name and a source are at most" & Natural'Image (Maximum_Text) &
      " characters");

   procedure Series (Argument : CCL.Host_Values.Value; Reply : in out CCL.Host_Values.Call_Result) is
      Object : Views.Snapshot;
      Captured : Boolean := False;
      Image : CCL.Objects.Image;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Bound.Selector, Argument.Object, Captured);
      end if;
      if not Captured then return; end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         Name : constant String := Views.Text (Object, Views.Field (Object, Root, M.Selector_Field'Pos (M.Selector_Name) + 1));
         Source : constant String :=
           Views.Text (Object, Views.Field (Object, Root, M.Selector_Field'Pos (M.Selector_Source) + 1));
         Answer : Pages.Page;
         Rows, Total, Skip : U64 := 0;
         Added : Boolean := True;
      begin
         if Too_Long (Name, Source) then
            Reply.Why := NAME_TOO_LONG;
            return;
         end if;
         M.Start (Bound.Series_List, Image);
         loop
            CCL_Metric_IO.List (Match_Of (Name), Stem (Name), Source, Skip, Answer, Rows, Total, Reply.Success,
                                Reply.Why);
            exit when not Reply.Success or else Rows = 0;
            for R in 0 .. Natural (Rows) - 1 loop
               declare
                  Base : constant Natural := R * Pages.List_Row_Words;
                  function Word (At_Word : Natural) return U64 is (Answer (Base + At_Word));
                  Row : constant M.Series_Row :=
                    (Id => Word (Pages.Row_Id),
                     Kind => Natural (U64'Min (U64'Max (Word (Pages.Row_Kind), 1), 3)) - 1,
                     Measure => Natural (U64'Min (U64'Max (Word (Pages.Row_Unit), 1), 5)) - 1,
                     Published => Word (Pages.Row_Published), Retained => Word (Pages.Row_Retained),
                     Shed => Word (Pages.Row_Shed), Evicted => Word (Pages.Row_Evicted),
                     First_Us => Word (Pages.Row_First), Last_Us => Word (Pages.Row_Last),
                     Last_Value => Word (Pages.Row_Last_Value));
               begin
                  exit when not M.Has_Room (Image);
                  M.Add_Series
                    (Image, Row, Pages.Get_Text (Answer, Base + Pages.Row_Name, Pages.Text_Bytes),
                     Pages.Get_Text (Answer, Base + Pages.Row_Source, Pages.Text_Bytes),
                     Pages.Get_Text (Answer, Base + Pages.Row_Program, Pages.Short_Text_Bytes),
                     Pages.Get_Text (Answer, Base + Pages.Row_Instance, Pages.Short_Text_Bytes), Added);
                  exit when not Added;
               end;
            end loop;
            Skip := Skip + Rows;
            exit when not Added or else not M.Has_Room (Image) or else Skip >= Total;
         end loop;
         if Reply.Success then
            Reply.Value := CCL.Host_Values.Object_Constant (Image);
         end if;
      end;
   end Series;

   --  The series a range reads: of those named exactly (and by source),
   --  the one published to last.
   procedure Pick (Name, Source : String; Id : out U64; Success : out Boolean; Why : out Failures.Failure) is
      Answer : Pages.Page;
      Rows, Total : U64;
      Latest : U64 := 0;
   begin
      Id := 0;
      CCL_Metric_IO.List (CCL_Metric_IO.Exact_Name, Name, Source, 0, Answer, Rows, Total, Success, Why);
      if not Success then return; end if;
      for R in 0 .. Natural (Rows) - 1 loop
         if Id = 0 or else Answer (R * Pages.List_Row_Words + Pages.Row_Last) >= Latest then
            Id := Answer (R * Pages.List_Row_Words + Pages.Row_Id);
            Latest := Answer (R * Pages.List_Row_Words + Pages.Row_Last);
         end if;
      end loop;
      if Rows = 0 then
         Success := False;
         Why := Failures.Failed
           (Failures.Not_Found, "no series named """ & Name & """" &
            (if Source'Length > 0 then " from " & Source else ""),
            "(metrics.series (Metric_Selector name => """" source => """")) lists them");
      end if;
   end Pick;

   procedure Read_Range (Argument : CCL.Host_Values.Value; Reply : in out CCL.Host_Values.Call_Result) is
      Object : Views.Snapshot;
      Captured : Boolean := False;
      Image : CCL.Objects.Image;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Bound.Range_Record, Argument.Object, Captured);
      end if;
      if not Captured then return; end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         function Field (Which : M.Range_Field) return Views.Cursor is
           (Views.Field (Object, Root, M.Range_Field'Pos (Which) + 1));
         Name : constant String := Views.Text (Object, Field (M.Range_Series));
         Source : constant String := Views.Text (Object, Field (M.Range_Source));
         From : constant U64 := Time_Of (CCL.Objects.Integer_Of (Views.Scalar (Object, Field (M.Range_From))));
         To : constant U64 := Time_Of (CCL.Objects.Integer_Of (Views.Scalar (Object, Field (M.Range_To))));
         Step_Value : constant Interfaces.Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Field (M.Range_Step)));
         Step : constant U64 := (if Step_Value > 0 then U64 (Step_Value) else 0);
         Id, Rows, Total, Gaps, Resolution, Skip, Shown_Last : U64 := 0;
         Answer : Pages.Page;
         Added : Boolean := True;
         First_Page : Boolean := True;
      begin
         if Too_Long (Name, Source) then
            Reply.Why := NAME_TOO_LONG;
            return;
         elsif From > To then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, "the Metric_Range ends before it starts", "give from <= to");
            return;
         end if;
         Pick (Name, Source, Id, Reply.Success, Reply.Why);
         if not Reply.Success then return; end if;
         M.Start (Bound.Items, Image);
         loop
            CCL_Metric_IO.Read_Range (Id, From, To, Skip, Step, Answer, Rows, Total, Gaps, Resolution,
                                      Reply.Success, Reply.Why);
            exit when not Reply.Success;
            if First_Page then
               for G in 0 .. Natural (Gaps) - 1 loop
                  declare
                     Base : constant Natural := Pages.Range_Gaps_At + G * Pages.Gap_Row_Words;
                  begin
                     M.Add_Gap (Image, Answer (Base), Answer (Base + 1), Answer (Base + 2),
                                (if Answer (Base + 3) = 2 then M.Store_Refused else M.Producer_Shed),
                                Added);
                  end;
               end loop;
               First_Page := False;
            end if;
            for R in 0 .. Natural (Rows) - 1 loop
               exit when not M.Has_Room (Image);
               if Step = 0 then
                  declare
                     Base : constant Natural := Pages.Range_Rows_At + R * Pages.Raw_Row_Words;
                  begin
                     M.Add_Point (Image, Answer (Base), Answer (Base + 1), Added);
                     Shown_Last := Answer (Base);
                  end;
               else
                  declare
                     Base : constant Natural := Pages.Range_Rows_At + R * Pages.Window_Row_Words;
                  begin
                     M.Add_Rollup
                       (Image, (Start_Us => Answer (Base), Count => Answer (Base + 1), Sum => Answer (Base + 2),
                                Minimum => Answer (Base + 3), Maximum => Answer (Base + 4),
                                Last => Answer (Base + 5), P50 => Answer (Base + 6), P99 => Answer (Base + 7)),
                        Added);
                     Shown_Last := Answer (Base);
                  end;
               end if;
               exit when not Added;
               Skip := Skip + 1;
            end loop;
            exit when Rows = 0 or else Skip >= Total or else not Added or else not M.Has_Room (Image);
         end loop;
         if Reply.Success and then Skip < Total then
            --  The rest is there: from just past the last shown.
            M.Add_Gap (Image, Shown_Last + 1, To, Total - Skip, M.Not_Shown, Added);
         end if;
         if Reply.Success then
            Reply.Value := CCL.Host_Values.Object_Constant (Image);
         end if;
      end;
   end Read_Range;

   procedure Window (Argument : CCL.Host_Values.Value; Reply : in out CCL.Host_Values.Call_Result) is
      Object : Views.Snapshot;
      Captured : Boolean := False;
      Image : CCL.Objects.Image;
      Built : Boolean;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Bound.Window_Record, Argument.Object, Captured);
      end if;
      if not Captured then return; end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         function Field (Which : M.Window_Field) return Views.Cursor is
           (Views.Field (Object, Root, M.Window_Field'Pos (Which) + 1));
         Name : constant String := Views.Text (Object, Field (M.Window_Series));
         Source : constant String := Views.Text (Object, Field (M.Window_Source));
         From : constant U64 := Time_Of (CCL.Objects.Integer_Of (Views.Scalar (Object, Field (M.Window_From))));
         To : constant U64 := Time_Of (CCL.Objects.Integer_Of (Views.Scalar (Object, Field (M.Window_To))));
         Answer : Pages.Page;
         Merged : U64;
      begin
         if Too_Long (Name, Source) then
            Reply.Why := NAME_TOO_LONG;
            return;
         elsif From > To then
            Reply.Why := Failures.Failed
              (Failures.Invalid_Argument, "the Metric_Window ends before it starts", "give from <= to");
            return;
         end if;
         CCL_Metric_IO.Read_Window (Match_Of (Name), Stem (Name), Source, From, To, Answer, Merged,
                                    Reply.Success, Reply.Why);
         if not Reply.Success then return; end if;
         M.Stats_Value
           (Bound.Stats,
            [Merged, Answer (Pages.Stat_Count), Answer (Pages.Stat_Sum), Answer (Pages.Stat_Minimum),
             Answer (Pages.Stat_Maximum), Answer (Pages.Stat_Last), Answer (Pages.Stat_P50),
             Answer (Pages.Stat_P90), Answer (Pages.Stat_P99), Answer (Pages.Stat_P999),
             Answer (Pages.Stat_Rate_Milli), Answer (Pages.Stat_Missing), Answer (Pages.Stat_Resolution),
             Answer (Pages.Stat_Error_Permille)],
            Image, Built);
         Reply.Success := Built;
         if Built then
            Reply.Value := CCL.Host_Values.Object_Constant (Image);
         end if;
      end;
   end Window;

   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result) is
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      case M.Operation'Val (Binding - M.FIRST_BINDING) is
         when M.Series => Series (Argument, Reply);
         when M.Range_Of => Read_Range (Argument, Reply);
         when M.Window => Window (Argument, Reply);
      end case;
   end Invoke;
end CCL_Metric_Bindings;
