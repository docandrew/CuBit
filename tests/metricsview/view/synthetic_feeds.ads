------------------------------------------------------------------------------
--  A Linux-hosted stand-in for metrics.svc's query path (Dash_Feeds.Feed):
--  series held in memory, a clock the test moves, and the store's answers
--  computed from the raw samples (tier windows of 1 s, 10 s, 1 min, 10 min
--  with exact quantiles, so the view's merges are checked against truth).
--  Not metrics.svc: no sketches, no retention, no scopes but a refusal
--  switch.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Charts_Columns;
with Charts_Format;
with Charts_Quantiles;
with Dash_Feeds;
with Dash_Model;

package Synthetic_Feeds is
   Maximum_Series : constant := 32;
   Maximum_Samples : constant := 400_000;
   --  The store's page sizes (CuBit.Metric_Query_Pages).
   Raw_Page : constant := 238;
   Window_Page : constant := 59;

   type Synthetic_Feed is new Dash_Feeds.Feed with private;

   procedure Add_Series
     (F : in out Synthetic_Feed; Name : String; Kind : Dash_Feeds.Series_Kind; Unit : Charts_Format.Unit);
   procedure Observe (F : in out Synthetic_Feed; Name : String; Time_Us, Value : Unsigned_64);
   procedure Set_Clock (F : in out Synthetic_Feed; Now : Unsigned_64);
   --  Every read of a series under Prefix answers Out_Of_Scope.
   procedure Refuse (F : in out Synthetic_Feed; Prefix : String);
   function Samples_Between (F : Synthetic_Feed; Name : String; From_Us, To_Us : Unsigned_64) return Natural;

   overriding procedure Find
     (F : in out Synthetic_Feed; Spec : Dash_Model.Series_Spec; Found : out Dash_Feeds.Series_Infos;
      Listed, Total : out Natural; Result : out Dash_Feeds.Status);
   overriding procedure List_All
     (F : in out Synthetic_Feed; Skip : Natural; Found : out Dash_Feeds.Series_Infos; Listed, Total : out Natural;
      Result : out Dash_Feeds.Status);
   overriding procedure Read_Raw
     (F : in out Synthetic_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip : Unsigned_64;
      Times, Values : out Charts_Quantiles.Sample_Array; Rows, Total : out Natural; Result : out Dash_Feeds.Status);
   overriding procedure Read_Windows
     (F : in out Synthetic_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip, Step_Us : Unsigned_64;
      Windows : out Charts_Columns.Window_Rows; Rows, Total : out Natural; Resolution_Us : out Unsigned_64;
      Result : out Dash_Feeds.Status);
   overriding procedure Read_Stats
     (F : in out Synthetic_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us : Unsigned_64;
      Stats : out Dash_Feeds.Window_Stats; Result : out Dash_Feeds.Status);
   overriding function Now_Us (F : Synthetic_Feed) return Unsigned_64;
   overriding function Calls (F : Synthetic_Feed) return Unsigned_64;
private
   type Sample_Access is access Charts_Quantiles.Sample_Array;
   type Series_Data is record
      Name : Dash_Model.Text;
      Kind : Dash_Feeds.Series_Kind := Dash_Feeds.Gauge;
      Unit : Charts_Format.Unit := Charts_Format.Count;
      Times, Values : Sample_Access;
      Used : Natural := 0;
   end record;
   type Series_Table is array (1 .. Maximum_Series) of Series_Data;
   type Synthetic_Feed is new Dash_Feeds.Feed with record
      Series : Series_Table;
      Count : Natural := 0;
      Clock : Unsigned_64 := 0;
      Refused : Dash_Model.Text;
      Call_Count : Unsigned_64 := 0;
   end record;
end Synthetic_Feeds;
