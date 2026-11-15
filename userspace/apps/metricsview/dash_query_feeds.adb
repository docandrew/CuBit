pragma Ada_2022;
with CuBit.Handles.Startup;
with CuBit.Metric_Protocol;
with CuBit.Metric_Updates;
with CuBit.Metric_Query_Pages;
with CuBit.Metrics;
with CuBit.Monotonic;
with Charts_Format;
with Dash_Sync;

package body Dash_Query_Feeds is
   package Queries renames CuBit.Metric_Queries;
   package Pages renames CuBit.Metric_Query_Pages;
   package Protocol renames CuBit.Metric_Protocol;
   use type Protocol.Status;
   use type Dash_Model.Match_Kind;

   Query_Series : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.query_us", CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);

   --  Row_Kind and Row_Unit: CuBit.Metric_Outlet's representations, from 1.
   Kinds : constant array (1 .. 3) of Dash_Feeds.Series_Kind :=
     [Dash_Feeds.Counter, Dash_Feeds.Gauge, Dash_Feeds.Distribution];
   Units : constant array (1 .. 5) of Charts_Format.Unit :=
     [Charts_Format.Count, Charts_Format.Microseconds, Charts_Format.Nanoseconds, Charts_Format.Bytes,
      Charts_Format.Pixels];

   function Status_Of (Result : Protocol.Status) return Dash_Feeds.Status is
     (case Result is
        when Protocol.OK => Dash_Feeds.OK,
        when Protocol.Not_Found => Dash_Feeds.Not_Found,
        when Protocol.Out_Of_Scope | Protocol.Denied => Dash_Feeds.Out_Of_Scope,
        when others => Dash_Feeds.Unavailable);

   --  One page: lent pages are large, kept off the stack.
   Answer : Pages.Page;

   procedure Bind (F : in out Query_Feed) is
   begin
      F.Reader := new Queries.Reader'
        (Queries.Bound_To (CuBit.Handles.Startup.Optional_Endpoint_Named (Queries.Observer_Binding)));
   end Bind;

   function Bound (F : Query_Feed) return Boolean is (F.Reader /= null and then Queries.Available (F.Reader.all));

   procedure Note_Call (F : in out Query_Feed; Started : Unsigned_64);
   procedure Note_Call (F : in out Query_Feed; Started : Unsigned_64) is
      Taken : constant Unsigned_64 := CuBit.Monotonic.Microseconds - Started;
   begin
      F.Call_Count := F.Call_Count + 1;
      F.Time_Us := F.Time_Us + Taken;
      F.Slowest_Us := Unsigned_64'Max (F.Slowest_Us, Taken);
      --  Called by the data thread with the shared lock let go; CuBit.Metrics
      --  is serialized by it.
      Dash_Sync.Lock;
      CuBit.Metrics.Observe (Query_Series, Taken);
      Dash_Sync.Unlock;
   end Note_Call;

   procedure List_Page
     (F : in out Query_Feed; Match : Queries.Name_Match; Name, Source : String; Skip : Natural;
      Found : out Dash_Feeds.Series_Infos; Listed, Total : out Natural; Result : out Dash_Feeds.Status);
   procedure List_Page
     (F : in out Query_Feed; Match : Queries.Name_Match; Name, Source : String; Skip : Natural;
      Found : out Dash_Feeds.Series_Infos; Listed, Total : out Natural; Result : out Dash_Feeds.Status)
   is
      Rows, All_Rows : Unsigned_64;
      Answered : Protocol.Status;
      Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      Found := [others => (others => <>)];
      Listed := 0;
      Total := 0;
      if not Bound (F) then
         Result := Dash_Feeds.Unavailable;
         return;
      end if;
      Queries.List (F.Reader.all, Match, Name, Source, Unsigned_64 (Skip), Answer, Rows, All_Rows, Answered);
      Note_Call (F, Started);
      Result := Status_Of (Answered);
      if Answered /= Protocol.OK then
         return;
      end if;
      Total := Natural (Unsigned_64'Min (All_Rows, Unsigned_64 (Natural'Last)));
      for R in 0 .. Natural (Rows) - 1 loop
         exit when Listed = Found'Length;
         declare
            Base : constant Natural := R * Pages.List_Row_Words;
            function Word (Offset : Natural) return Unsigned_64 is (Answer (Base + Offset));
            Kind : constant Unsigned_64 := Word (Pages.Row_Kind);
            Unit : constant Unsigned_64 := Word (Pages.Row_Unit);
         begin
            Listed := Listed + 1;
            Found (Found'First + Listed - 1) :=
              (Id => Word (Pages.Row_Id),
               Name => Dash_Model.To_Text (Pages.Get_Text (Answer, Base + Pages.Row_Name, Pages.Text_Bytes)),
               Source => Dash_Model.To_Text (Pages.Get_Text (Answer, Base + Pages.Row_Source, Pages.Text_Bytes)),
               Kind => (if Kind in 1 .. 3 then Kinds (Natural (Kind)) else Dash_Feeds.Gauge),
               Unit => (if Unit in 1 .. 5 then Units (Natural (Unit)) else Charts_Format.Count),
               Last_Us => Word (Pages.Row_Last),
               Raw_From_Us => Word (Pages.Row_Raw_From),
               First_Us => Word (Pages.Row_First),
               Published => Word (Pages.Row_Published));
         end;
      end loop;
   end List_Page;

   overriding procedure Find
     (F : in out Query_Feed; Spec : Dash_Model.Series_Spec; Found : out Dash_Feeds.Series_Infos;
      Listed, Total : out Natural; Result : out Dash_Feeds.Status) is
   begin
      List_Page (F, (if Spec.Match = Dash_Model.Prefix then Queries.Name_Prefix else Queries.Exact_Name),
                 Dash_Model.Image (Spec.Name), Dash_Model.Image (Spec.Source), 0, Found, Listed, Total, Result);
   end Find;

   overriding procedure List_All
     (F : in out Query_Feed; Skip : Natural; Found : out Dash_Feeds.Series_Infos; Listed, Total : out Natural;
      Result : out Dash_Feeds.Status) is
   begin
      List_Page (F, Queries.Any_Name, "", "", Skip, Found, Listed, Total, Result);
   end List_All;

   overriding procedure Read_Raw
     (F : in out Query_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip : Unsigned_64;
      Times, Values : out Charts_Quantiles.Sample_Array; Rows, Total : out Natural; Result : out Dash_Feeds.Status)
   is
      Count, All_Rows, Gaps, Resolution : Unsigned_64;
      Answered : Protocol.Status;
      Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      Times := [others => 0];
      Values := [others => 0];
      Rows := 0;
      Total := 0;
      if not Bound (F) then
         Result := Dash_Feeds.Unavailable;
         return;
      end if;
      Queries.Read_Range (F.Reader.all, Id, From_Us, To_Us, Skip, 0, Answer, Count, All_Rows, Gaps, Resolution,
                          Answered);
      Note_Call (F, Started);
      Result := Status_Of (Answered);
      if Answered /= Protocol.OK then
         return;
      end if;
      --  All_Rows counts the range; Skip of them were taken before.
      Total := Natural (Unsigned_64'Min (All_Rows, Unsigned_64 (Natural'Last)));
      Rows := Natural (Unsigned_64'Min (Count, Unsigned_64 (Times'Length)));
      for R in 0 .. Rows - 1 loop
         Times (Times'First + R) := Answer (Pages.Range_Rows_At + R * Pages.Raw_Row_Words);
         Values (Values'First + R) := Answer (Pages.Range_Rows_At + R * Pages.Raw_Row_Words + 1);
      end loop;
   end Read_Raw;

   overriding procedure Read_Windows
     (F : in out Query_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip, Step_Us : Unsigned_64;
      Windows : out Charts_Columns.Window_Rows; Rows, Total : out Natural; Resolution_Us : out Unsigned_64;
      Result : out Dash_Feeds.Status)
   is
      Count, All_Rows, Gaps : Unsigned_64;
      Answered : Protocol.Status;
      Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      Windows := [others => (others => <>)];
      Rows := 0;
      Total := 0;
      Resolution_Us := 0;
      if not Bound (F) then
         Result := Dash_Feeds.Unavailable;
         return;
      end if;
      Queries.Read_Range (F.Reader.all, Id, From_Us, To_Us, Skip, Step_Us, Answer, Count, All_Rows, Gaps,
                          Resolution_Us, Answered);
      Note_Call (F, Started);
      Result := Status_Of (Answered);
      if Answered /= Protocol.OK then
         return;
      end if;
      Total := Natural (Unsigned_64'Min (All_Rows, Unsigned_64 (Natural'Last)));
      Rows := Natural (Unsigned_64'Min (Count, Unsigned_64 (Windows'Length)));
      for R in 0 .. Rows - 1 loop
         declare
            Base : constant Natural := Pages.Range_Rows_At + R * Pages.Window_Row_Words;
         begin
            Windows (Windows'First + R) :=
              (Start_Us => Answer (Base), Count => Answer (Base + 1), Sum => Answer (Base + 2),
               Minimum => Answer (Base + 3), Maximum => Answer (Base + 4), Last => Answer (Base + 5),
               P50 => Answer (Base + 6), P99 => Answer (Base + 7));
         end;
      end loop;
   end Read_Windows;

   overriding procedure Read_Stats
     (F : in out Query_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us : Unsigned_64;
      Stats : out Dash_Feeds.Window_Stats; Result : out Dash_Feeds.Status)
   is
      Merged : Unsigned_64;
      Answered : Protocol.Status;
      Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      Stats := (others => 0);
      if not Bound (F) then
         Result := Dash_Feeds.Unavailable;
         return;
      end if;
      Queries.Read_Window (F.Reader.all, Id, Queries.Any_Name, "", "", From_Us, To_Us, Answer, Merged, Answered);
      Note_Call (F, Started);
      Result := Status_Of (Answered);
      if Answered /= Protocol.OK then
         return;
      end if;
      Stats := (Count => Answer (Pages.Stat_Count), Sum => Answer (Pages.Stat_Sum),
                Minimum => Answer (Pages.Stat_Minimum), Maximum => Answer (Pages.Stat_Maximum),
                Last => Answer (Pages.Stat_Last), P50 => Answer (Pages.Stat_P50), P90 => Answer (Pages.Stat_P90),
                P99 => Answer (Pages.Stat_P99), P999 => Answer (Pages.Stat_P999),
                Rate_Milli => Answer (Pages.Stat_Rate_Milli), Missing => Answer (Pages.Stat_Missing));
   end Read_Stats;

   overriding procedure Start_Push (F : in out Query_Feed; Started : out Boolean) is
      Answered : Protocol.Status;
      Begun : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      Started := False;
      if not Bound (F) or else F.Push then
         Started := F.Push;
         return;
      end if;
      Queries.Subscribe (F.Reader.all, Queries.Any_Name, "", "", F.Link, Answered);
      Note_Call (F, Begun);
      F.Push := Answered = Protocol.OK;
      Started := F.Push;
   end Start_Push;

   overriding function Pushing (F : Query_Feed) return Boolean is (F.Push and then F.Link.Active);

   overriding procedure Take_Updates (F : in out Query_Feed; Into : out Dash_Feeds.Update_Array; Count : out Natural)
   is
      use type CuBit.Channels.Take_Result;
      package U renames CuBit.Metric_Updates;
      Wire : U.Element_Buffer;
      Length : Natural;
      Taken : CuBit.Channels.Take_Result;
   begin
      Count := 0;
      if not Pushing (F) then
         return;
      end if;
      while Count < Into'Length loop
         CuBit.Channels.Take (F.Link, Wire'Address, Wire'Length, Length, Taken);
         exit when Taken /= CuBit.Channels.Taken and then Taken /= CuBit.Channels.Malformed;
         declare
            Item : constant U.Decoded := (if Taken = CuBit.Channels.Taken and then Length = Wire'Length
                                          then U.Decode (Wire) else (Success => False));
         begin
            if not Item.Success then
               F.Malformed := F.Malformed + 1;
            else
               Count := Count + 1;
               Into (Into'First + Count - 1) :=
                 (case Item.Value.Kind is
                    when U.Sample =>
                      (Kind => Dash_Feeds.Sample, Series => Item.Value.Series, Time_Us => Item.Value.Time_Us,
                       Value => Item.Value.Value, others => 0),
                    when U.Gap =>
                      (Kind => Dash_Feeds.Gap, Series => Item.Value.Series, Missing => Item.Value.Missing,
                       First_Us => Item.Value.First_Us, Last_Us => Item.Value.Last_Us, others => 0));
            end if;
         end;
      end loop;
   end Take_Updates;

   overriding function Now_Us (F : Query_Feed) return Unsigned_64 is (CuBit.Monotonic.Microseconds);
   overriding function Calls (F : Query_Feed) return Unsigned_64 is (F.Call_Count);
   function Call_Time (F : Query_Feed) return Unsigned_64 is (F.Time_Us);
   function Slowest_Call (F : Query_Feed) return Unsigned_64 is (F.Slowest_Us);
end Dash_Query_Feeds;
