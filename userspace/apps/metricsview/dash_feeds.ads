------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The metrics viewer's data layer (MET-002's app): what the tracks and the
--  view read series through. Today's feed asks metrics.svc's query path
--  (Dash_Query_Feeds); a push subscription (DESIGN-metrics-push.md) is
--  another Feed, and nothing that draws changes.
--
--  @description
--  Reads are by range and paged (Skip, Total) as the store answers them.
--  Read_Raw gives samples in the store's order (not necessarily by time);
--  Read_Windows gives tier windows oldest first, of the coarsest tier no
--  wider than Step_Us. A Feed belongs to the program's one UI task.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Charts_Columns;
with Charts_Format;
with Charts_Quantiles;
with Dash_Model;

package Dash_Feeds is
   type Status is (OK, Not_Found, Out_Of_Scope, Unavailable);

   subtype Series_Id is Unsigned_64;
   No_Series : constant Series_Id := 0;

   type Series_Kind is (Counter, Gauge, Distribution);

   type Series_Info is record
      Id : Series_Id := No_Series;
      Name, Source : Dash_Model.Text;
      Kind : Series_Kind := Gauge;
      Unit : Charts_Format.Unit := Charts_Format.Count;
      --  The newest sample's time, and the oldest raw sample still held
      --  (older ranges have only tier windows).
      Last_Us, Raw_From_Us : Unsigned_64 := 0;
   end record;
   type Series_Infos is array (Positive range <>) of Series_Info;

   --  A window's summary (metrics.svc's Read_Window: the selected series
   --  merged, their sketches added).
   type Window_Stats is record
      Count, Sum, Minimum, Maximum, Last, P50, P90, P99, P999 : Unsigned_64 := 0;
      --  Per second, in thousandths: a counter's increments, others' samples.
      Rate_Milli : Unsigned_64 := 0;
      --  Samples the store knows it lacks (shed, refused) in the window.
      Missing : Unsigned_64 := 0;
   end record;

   type Feed is abstract tagged limited null record;

   --  The series Spec names that the reader may see: Found holds up to its
   --  length of them, Listed how many, Total how many matched in all.
   procedure Find
     (F : in out Feed; Spec : Dash_Model.Series_Spec; Found : out Series_Infos; Listed, Total : out Natural;
      Result : out Status) is abstract;

   --  Raw samples of Id with times From_Us .. To_Us, skipping the first
   --  Skip of them: Rows of them into Times and Values (at most their
   --  length); Total in the range in all.
   procedure Read_Raw
     (F : in out Feed; Id : Series_Id; From_Us, To_Us, Skip : Unsigned_64;
      Times, Values : out Charts_Quantiles.Sample_Array; Rows, Total : out Natural; Result : out Status)
   is abstract;

   procedure Read_Windows
     (F : in out Feed; Id : Series_Id; From_Us, To_Us, Skip, Step_Us : Unsigned_64;
      Windows : out Charts_Columns.Window_Rows; Rows, Total : out Natural; Resolution_Us : out Unsigned_64;
      Result : out Status) is abstract;

   procedure Read_Stats
     (F : in out Feed; Id : Series_Id; From_Us, To_Us : Unsigned_64; Stats : out Window_Stats; Result : out Status)
   is abstract;

   --  The clock samples are stamped with (CuBit.Monotonic on CuBit).
   function Now_Us (F : Feed) return Unsigned_64 is abstract;

   --  What the feed costs, for the status line and the app's own metrics.
   function Calls (F : Feed) return Unsigned_64 is abstract;
end Dash_Feeds;
