------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a panel's source picker offers (MET-002's app): the views a panel
--  can show its series as (each a Panel_Kind of the Dashboard type with its
--  settings), the series it can read (one series by name, or every series
--  under a name prefix), and the panel a choice makes. Platform-free and
--  without state: the window's picker and the tests use the same rules.
--
--  @description
--  Every panel these make is a value the Dashboard type accepts (sizes,
--  statistics, buckets in their ranges), so a picked panel saves, copies
--  and reloads as any other.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Dash_Model;

package Dash_Choices is
   --  The views, as the picker lists them.
   type View_Choice is
     (Lines_Median_And_Tail, Line_Tail, Line_Mean, Line_Maximum, Line_Rate,
      Heatmap_Tail, Heatmap_Median,
      Histogram_Of_Values,
      Number_Tail, Number_Median, Number_Last, Number_Count, Number_Rate);
   function Caption (Choice : View_Choice) return String;
   --  The view's short name (a picked panel's title ends with it).
   function Short_Name (Choice : View_Choice) return String;

   --  The view a panel shows, when it is one of the choices exactly (its
   --  CCL may hold another mix of lines, say): Found False then.
   procedure View_Of (P : Dash_Model.Panel; Choice : out View_Choice; Found : out Boolean);

   --  P shown as Choice. Its series, size and title stay; a budget stays
   --  where the view is in the series' unit (counts and rates have none).
   --  A histogram it was keeps its buckets; a new one gets
   --  Histogram_Buckets of them from 0 to Value_Top (the values the panel
   --  shows now; 0: none known, one unit each).
   Histogram_Buckets : constant := 10;
   function With_View (P : Dash_Model.Panel; Choice : View_Choice; Value_Top : Unsigned_64) return Dash_Model.Panel;

   --  A source: one series by its exact name (of any source), or every
   --  series whose name starts with Name.
   type Source_Choice is record
      Name : Dash_Model.Text;
      Match : Dash_Model.Match_Kind := Dash_Model.Exact;
   end record;
   function Caption (Source : Source_Choice) return String;
   --  The panel's source when it reads exactly one: Found False when it
   --  reads several (a heatmap's rows) or none.
   procedure Source_Of (P : Dash_Model.Panel; Source : out Source_Choice; Found : out Boolean);
   --  P reading Source alone. Its title follows when it was the one a
   --  picker gave it (Title_For) or the new panel's.
   function With_Source (P : Dash_Model.Panel; Source : Source_Choice) return Dash_Model.Panel;

   --  The title a picked panel gets: its source and view.
   function Title_For (P : Dash_Model.Panel) return String;

   --  A panel added to a dashboard: no series yet (the picker opens on
   --  it), a line view, New_Width by New_Height grid units.
   New_Title : constant String := "New panel";
   New_Width : constant Dash_Model.Grid_Width := 4;
   New_Height : constant Dash_Model.Grid_Height := 3;
   function New_Panel return Dash_Model.Panel;

   --  A new dashboard: one new panel, five minutes, live.
   New_Dashboard_Title : constant String := "New dashboard";
   New_Span_Ms : constant Dash_Model.Span_Ms := 300_000;
   function New_Dashboard (Title : String) return Dash_Model.Dashboard;

   --  The prefix a series name groups under: its name up to and with its
   --  last '.', or "" without one.
   function Group_Of (Name : String) return String;
end Dash_Choices;
