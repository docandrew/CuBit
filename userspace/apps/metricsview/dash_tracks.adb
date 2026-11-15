pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Charts_Quantiles;

package body Dash_Tracks is
   use type Dash_Feeds.Status;
   use type Dash_Model.Match_Kind;
   use type Charts_Columns.Summary;

   subtype Sample_Array is Charts_Quantiles.Sample_Array;
   type Sample_Access is access Sample_Array;

   --  One store page (CuBit.Metric_Query_Pages: 238 raw rows, 59 windows).
   Page_Rows : constant := 256;
   Matches_Kept : constant := 16;
   Initial_Batch : constant := 4_096;

   --  Scratch shared by every track (one UI task): grown by doubling up to
   --  Maximum_Batch_Samples, never shrunk.
   Batch_Times, Batch_Values, Grouped : Sample_Access;
   Page_Times, Page_Values : Sample_Array (1 .. Page_Rows);
   Window_Batch : Charts_Columns.Window_Rows (1 .. Page_Rows);
   Fetch_Count, Folded_Count : Unsigned_64 := 0;

   procedure Free is new Ada.Unchecked_Deallocation (Sample_Array, Sample_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Charts_Columns.Ring, Column_Ring_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Charts_Buckets.Ring, Bucket_Ring_Access);

   function Fetches return Unsigned_64 is (Fetch_Count);
   function Samples_Folded return Unsigned_64 is (Folded_Count);

   --  Room for Needed samples in Item (contents kept); False at the ceiling.
   procedure Reserve (Item : in out Sample_Access; Needed : Natural; Ok : out Boolean);
   procedure Reserve (Item : in out Sample_Access; Needed : Natural; Ok : out Boolean) is
      Size : Natural := (if Item = null then Initial_Batch else Item'Length);
   begin
      Ok := Needed <= Maximum_Batch_Samples;
      if not Ok or else (Item /= null and then Item'Length >= Needed) then
         return;
      end if;
      while Size < Needed loop
         Size := Natural'Min (Size * 2, Maximum_Batch_Samples);
      end loop;
      declare
         Grown : constant Sample_Access := new Sample_Array (0 .. Size - 1);
      begin
         if Item /= null then
            Grown (0 .. Item'Length - 1) := Item.all;
            Free (Item);
         end if;
         Item := Grown;
      end;
   end Reserve;

   procedure Mark (T : in out Track; C : Column_Number);
   procedure Mark (T : in out Track; C : Column_Number) is
   begin
      if not T.Dirty then
         T.Dirty := True;
         T.Dirty_First := C;
         T.Dirty_Last := C;
      else
         T.Dirty_First := Unsigned_64'Min (T.Dirty_First, C);
         T.Dirty_Last := Unsigned_64'Max (T.Dirty_Last, C);
      end if;
   end Mark;

   procedure Configure (T : in out Track; V : Axis.Viewport; Histogram : Boolean; Layout : Charts_Buckets.Layout) is
   begin
      if T.Columns /= null then
         Free (T.Columns);
      end if;
      if T.Buckets /= null then
         Free (T.Buckets);
      end if;
      T.Columns := new Charts_Columns.Ring (V.Columns);
      Charts_Columns.Reset (T.Columns.all, V.Last);
      if Histogram then
         T.Buckets := new Charts_Buckets.Ring (V.Columns, Layout.Count);
         Charts_Buckets.Reset (T.Buckets.all, V.Last);
      end if;
      T.Layout := Layout;
      T.Width_Us := V.Width;
      T.Lo := 0;
      T.Next := 0;
      T.Stats_Stale := True;
   end Configure;

   procedure Resolve (T : in out Track; F : in out Dash_Feeds.Feed'Class; Now_Us : Unsigned_64) is
      Found : Dash_Feeds.Series_Infos (1 .. Matches_Kept);
      Listed, Total : Natural;
      Result : Dash_Feeds.Status;
      Chosen : Natural := 0;
   begin
      if T.State = Resolved or else Now_Us < T.Next_Resolve_Us then
         return;
      end if;
      T.Next_Resolve_Us := Now_Us + Resolve_Interval_Us;
      F.Find (T.Spec, Found, Listed, Total, Result);
      Fetch_Count := Fetch_Count + 1;
      case Result is
         when Dash_Feeds.OK =>
            if T.Spec.Match = Dash_Model.Prefix then
               Chosen := (if T.Match_Index <= Listed then T.Match_Index else 0);
            else
               --  Several instances of one name (a restarted producer): the
               --  one that published last.
               for I in 1 .. Listed loop
                  if Chosen = 0 or else Found (I).Last_Us > Found (Chosen).Last_Us then
                     Chosen := I;
                  end if;
               end loop;
            end if;
            if Chosen = 0 then
               T.State := Missing;
            else
               T.Info := Found (Chosen);
               T.State := Resolved;
               T.Lo := 0;
               T.Next := 0;
            end if;
         when Dash_Feeds.Not_Found => T.State := Missing;
         when Dash_Feeds.Out_Of_Scope => T.State := Refused;
         when Dash_Feeds.Unavailable => T.State := Unavailable;
      end case;
   end Resolve;

   procedure Follow (T : in out Track; V : Axis.Viewport) is
   begin
      if T.Columns = null then
         return;
      end if;
      if V.Width /= T.Width_Us or else V.Columns /= T.Columns.Capacity then
         declare
            Old : Column_Ring_Access := T.Columns;
         begin
            T.Columns := new Charts_Columns.Ring (V.Columns);
            T.Columns.Last := V.Last;
            Charts_Columns.Resample (Old.all, T.Width_Us, T.Columns.all, V.Width);
            Free (Old);
         end;
         if T.Buckets /= null then
            declare
               Count : constant Charts_Buckets.Bucket_Count := T.Buckets.Buckets;
            begin
               Free (T.Buckets);
               T.Buckets := new Charts_Buckets.Ring (V.Columns, Count);
               Charts_Buckets.Reset (T.Buckets.all, V.Last);
            end;
         end if;
         T.Width_Us := V.Width;
         T.Lo := 0;
         T.Next := 0;
         Mark (T, Axis.First (V));
         Mark (T, V.Last);
      else
         Charts_Columns.Move_To (T.Columns.all, V.Last);
         if T.Buckets /= null then
            Charts_Buckets.Move_To (T.Buckets.all, V.Last);
         end if;
         --  What is held of the final run.
         T.Lo := Unsigned_64'Max (T.Lo, Axis.First (V));
         T.Next := Unsigned_64'Min (T.Next, V.Last + 1);
         if T.Lo >= T.Next then
            T.Lo := 0;
            T.Next := 0;
         end if;
      end if;
      T.Stats_Stale := True;
      T.Previous_Due_Us := 0;
   end Follow;

   function Loaded (T : Track) return Boolean is (T.Lo < T.Next);

   --  Columns C0 .. C1 from the store: their summaries replaced (dirty
   --  where changed). False when the feed refused.
   procedure Fetch (T : in out Track; F : in out Dash_Feeds.Feed'Class; C0, C1 : Column_Number; Ok : out Boolean);
   procedure Fetch (T : in out Track; F : in out Dash_Feeds.Feed'Class; C0, C1 : Column_Number; Ok : out Boolean)
   is
      W : constant Axis.Column_Width := T.Width_Us;
      From_Us : constant Unsigned_64 := Axis.Start_Of (C0, W);
      To_Us : constant Unsigned_64 := Axis.End_Of (C1, W);
      Span : constant Natural := Natural (C1 - C0) + 1;
      Result : Dash_Feeds.Status := Dash_Feeds.OK;

      procedure Replace (C : Column_Number; S : Charts_Columns.Summary);
      procedure Replace (C : Column_Number; S : Charts_Columns.Summary) is
      begin
         if Charts_Columns.Get (T.Columns.all, C) /= S then
            Charts_Columns.Put (T.Columns.all, C, S);
            Mark (T, C);
         end if;
      end Replace;

      procedure Raw is
         Gathered : Natural := 0;
         Rows, Total : Natural;
         Room : Boolean;
         Offsets : array (0 .. Span) of Natural := [others => 0];
         Latest_Time : array (0 .. Span - 1) of Unsigned_64 := [others => 0];
         Latest_Value : array (0 .. Span - 1) of Unsigned_64 := [others => 0];
      begin
         loop
            F.Read_Raw (T.Info.Id, From_Us, To_Us, Unsigned_64 (Gathered), Page_Times, Page_Values, Rows, Total,
                        Result);
            Fetch_Count := Fetch_Count + 1;
            exit when Result /= Dash_Feeds.OK or else Rows = 0;
            Reserve (Batch_Times, Gathered + Rows, Room);
            Reserve (Batch_Values, Gathered + Rows, Room);
            exit when not Room;
            Batch_Times (Gathered .. Gathered + Rows - 1) := Page_Times (1 .. Rows);
            Batch_Values (Gathered .. Gathered + Rows - 1) := Page_Values (1 .. Rows);
            Gathered := Gathered + Rows;
            exit when Gathered >= Total;
         end loop;
         if Result /= Dash_Feeds.OK then
            return;
         end if;
         --  Counting sort by column: samples need not come in time order.
         for I in 0 .. Gathered - 1 loop
            declare
               C : constant Column_Number := Axis.Column_Of (Batch_Times (I), W);
            begin
               if C in C0 .. C1 then
                  declare
                     K : constant Natural := Natural (C - C0);
                  begin
                     Offsets (K + 1) := Offsets (K + 1) + 1;
                     if Batch_Times (I) >= Latest_Time (K) then
                        Latest_Time (K) := Batch_Times (I);
                        Latest_Value (K) := Batch_Values (I);
                     end if;
                  end;
               end if;
            end;
         end loop;
         for K in 1 .. Span loop
            Offsets (K) := Offsets (K) + Offsets (K - 1);
         end loop;
         Reserve (Grouped, Natural'Max (Offsets (Span), 1), Room);
         declare
            Fill : array (0 .. Span - 1) of Natural;
         begin
            for K in 0 .. Span - 1 loop
               Fill (K) := Offsets (K);
            end loop;
            for I in 0 .. Gathered - 1 loop
               declare
                  C : constant Column_Number := Axis.Column_Of (Batch_Times (I), W);
               begin
                  if C in C0 .. C1 then
                     Grouped (Fill (Natural (C - C0))) := Batch_Values (I);
                     Fill (Natural (C - C0)) := Fill (Natural (C - C0)) + 1;
                  end if;
               end;
            end loop;
         end;
         for K in 0 .. Span - 1 loop
            declare
               C : constant Column_Number := C0 + Unsigned_64 (K);
               S : Charts_Columns.Summary := Charts_Columns.Nothing;
            begin
               if Offsets (K + 1) > Offsets (K) then
                  Charts_Columns.Summarize_Raw (Grouped (Offsets (K) .. Offsets (K + 1) - 1), Latest_Value (K), S);
               end if;
               if T.Buckets /= null then
                  Charts_Buckets.Clear (T.Buckets.all, C);
                  for I in Offsets (K) .. Offsets (K + 1) - 1 loop
                     Charts_Buckets.Add (T.Buckets.all, T.Layout, C, Grouped (I));
                  end loop;
               end if;
               Replace (C, S);
            end;
         end loop;
         Folded_Count := Folded_Count + Unsigned_64 (Gathered);
         T.From_Windows := False;
      end Raw;

      procedure Windows is
         Skip : Natural := 0;
         Rows, Total : Natural;
         Resolution : Unsigned_64;
         Group : Charts_Columns.Window_Rows (1 .. Charts_Columns.Maximum_Column_Windows);
         Used : Natural := 0;
         Group_Column : Column_Number := C0;
         Next_Column : Column_Number := C0;

         procedure Put_Empty_Until (C : Column_Number);
         procedure Put_Empty_Until (C : Column_Number) is
         begin
            while Next_Column < C and then Next_Column <= C1 loop
               if T.Buckets /= null then
                  Charts_Buckets.Clear (T.Buckets.all, Next_Column);
               end if;
               Replace (Next_Column, Charts_Columns.Nothing);
               Next_Column := Next_Column + 1;
            end loop;
         end Put_Empty_Until;

         procedure Flush;
         procedure Flush is
         begin
            if Used = 0 then
               return;
            end if;
            Put_Empty_Until (Group_Column);
            if T.Buckets /= null then
               Charts_Buckets.Clear (T.Buckets.all, Group_Column);
               for I in 1 .. Used loop
                  Charts_Buckets.Add_Window (T.Buckets.all, T.Layout, Group_Column, Group (I));
               end loop;
            end if;
            Replace (Group_Column, Charts_Columns.Summarize_Windows (Group (1 .. Used)));
            Next_Column := Group_Column + 1;
            Used := 0;
         end Flush;
      begin
         loop
            F.Read_Windows (T.Info.Id, From_Us, To_Us, Unsigned_64 (Skip), W, Window_Batch, Rows, Total, Resolution,
                            Result);
            Fetch_Count := Fetch_Count + 1;
            exit when Result /= Dash_Feeds.OK or else Rows = 0;
            for I in 1 .. Rows loop
               declare
                  Row : constant Charts_Columns.Window_Row := Window_Batch (I);
                  C : constant Column_Number := Axis.Column_Of (Unsigned_64'Max (Row.Start_Us, From_Us), W);
               begin
                  if C in C0 .. C1 and then C >= Next_Column then
                     if Used > 0 and then (C /= Group_Column or else Used = Group'Length) then
                        Flush;
                     end if;
                     if C >= Next_Column then
                        Group_Column := C;
                        Used := Used + 1;
                        Group (Used) := Row;
                     end if;
                  end if;
               end;
            end loop;
            Skip := Skip + Rows;
            Folded_Count := Folded_Count + Unsigned_64 (Rows);
            exit when Skip >= Total;
         end loop;
         if Result = Dash_Feeds.OK then
            Flush;
            Put_Empty_Until (C1 + 1);
         end if;
         T.From_Windows := True;
      end Windows;
   begin
      if W < Finest_Tier_Us then
         Raw;
      else
         Windows;
      end if;
      Ok := Result = Dash_Feeds.OK;
      if not Ok then
         T.State := (case Result is
                       when Dash_Feeds.Out_Of_Scope => Refused,
                       when Dash_Feeds.Not_Found => Unresolved,
                       when others => Unavailable);
      end if;
   end Fetch;

   procedure Refresh
     (T : in out Track; F : in out Dash_Feeds.Feed'Class; V : Axis.Viewport; Now_Us, Budget_End_Us : Unsigned_64)
   is
      Horizon : constant Column_Number :=
        Axis.Column_Of ((if Now_Us > Settle_Margin_Us then Now_Us - Settle_Margin_Us else 0), V.Width);
      First : constant Column_Number := Axis.First (V);
      Ok : Boolean := True;
   begin
      if T.State /= Resolved or else T.Columns = null then
         return;
      end if;
      if not Loaded (T) then
         --  Start the final run where provisional columns begin.
         declare
            Start : constant Column_Number := Unsigned_64'Max (First, Unsigned_64'Min (Horizon, V.Last + 1));
         begin
            T.Lo := Start;
            T.Next := Start;
         end;
      end if;
      --  The right: the live edge, and what panning right uncovered.
      if T.Next <= V.Last then
         declare
            R0 : constant Column_Number := T.Next;
            R1 : constant Column_Number :=
              (if V.Last - R0 < 2 * Chunk_Columns then V.Last else R0 + Chunk_Columns - 1);
         begin
            Fetch (T, F, R0, R1, Ok);
            if Ok then
               T.Next := Unsigned_64'Max (T.Next, Unsigned_64'Min (R1 + 1, Horizon));
            end if;
         end;
      end if;
      --  The left, oldest last, while the budget lasts.
      while Ok and then T.Lo > First and then F.Now_Us < Budget_End_Us loop
         declare
            L0 : constant Column_Number :=
              (if T.Lo - First > Chunk_Columns then T.Lo - Chunk_Columns else First);
         begin
            Fetch (T, F, L0, T.Lo - 1, Ok);
            if Ok then
               T.Lo := L0;
            end if;
         end;
      end loop;
      if T.Dirty then
         T.Stats_Stale := True;
      end if;
   end Refresh;

   function Complete (T : Track; V : Axis.Viewport) return Boolean is
     (T.State /= Resolved or else (T.Lo <= Axis.First (V) and then T.Next >= V.Last));

   procedure Refresh_Stats (T : in out Track; F : in out Dash_Feeds.Feed'Class; V : Axis.Viewport; Now_Us : Unsigned_64)
   is
      Result : Dash_Feeds.Status;
      Left : constant Unsigned_64 := Axis.Left_Time (V);
      Length : constant Unsigned_64 := Axis.Right_Time (V) - Left + 1;
   begin
      if T.State /= Resolved then
         return;
      end if;
      if T.Stats_Stale then
         F.Read_Stats (T.Info.Id, Left, Axis.Right_Time (V), T.Stats, Result);
         Fetch_Count := Fetch_Count + 1;
         T.Stats_Valid := Result = Dash_Feeds.OK;
         T.Stats_Stale := False;
      end if;
      if Now_Us >= T.Previous_Due_Us then
         T.Previous_Due_Us := Now_Us + Previous_Interval_Us;
         T.Previous_Valid := False;
         if Left >= Length then
            F.Read_Stats (T.Info.Id, Left - Length, Left - 1, T.Previous, Result);
            Fetch_Count := Fetch_Count + 1;
            T.Previous_Valid := Result = Dash_Feeds.OK;
         end if;
      end if;
   end Refresh_Stats;

   procedure Take_Dirty (T : in out Track; Any : out Boolean; First, Last : out Column_Number) is
   begin
      Any := T.Dirty;
      First := T.Dirty_First;
      Last := T.Dirty_Last;
      T.Dirty := False;
   end Take_Dirty;
end Dash_Tracks;
