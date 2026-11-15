pragma Ada_2022;

package body Synthetic_Feeds is
   use type Dash_Model.Match_Kind;
   package Q renames Charts_Quantiles;

   Tier_Widths : constant array (1 .. 4) of Unsigned_64 := [1_000_000, 10_000_000, 60_000_000, 600_000_000];

   function Index_Of (F : Synthetic_Feed; Name : String) return Natural is
   begin
      for I in 1 .. F.Count loop
         if Dash_Model.Image (F.Series (I).Name) = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Of;

   procedure Add_Series
     (F : in out Synthetic_Feed; Name : String; Kind : Dash_Feeds.Series_Kind; Unit : Charts_Format.Unit) is
   begin
      F.Count := F.Count + 1;
      F.Series (F.Count) := (Name => Dash_Model.To_Text (Name), Kind => Kind, Unit => Unit,
                             Times => new Q.Sample_Array (1 .. Maximum_Samples),
                             Values => new Q.Sample_Array (1 .. Maximum_Samples), Used => 0);
   end Add_Series;

   procedure Observe (F : in out Synthetic_Feed; Name : String; Time_Us, Value : Unsigned_64) is
      I : constant Natural := Index_Of (F, Name);
   begin
      if I = 0 or else F.Series (I).Used = Maximum_Samples then
         return;
      end if;
      F.Series (I).Used := F.Series (I).Used + 1;
      F.Series (I).Times (F.Series (I).Used) := Time_Us;
      F.Series (I).Values (F.Series (I).Used) := Value;
   end Observe;

   procedure Set_Clock (F : in out Synthetic_Feed; Now : Unsigned_64) is
   begin
      F.Clock := Now;
   end Set_Clock;

   procedure Refuse (F : in out Synthetic_Feed; Prefix : String) is
   begin
      F.Refused := Dash_Model.To_Text (Prefix);
   end Refuse;

   function Refused (F : Synthetic_Feed; Name : String) return Boolean is
     (F.Refused.Length > 0 and then Name'Length >= F.Refused.Length and then
      Name (Name'First .. Name'First + F.Refused.Length - 1) = Dash_Model.Image (F.Refused));

   function Samples_Between (F : Synthetic_Feed; Name : String; From_Us, To_Us : Unsigned_64) return Natural is
      I : constant Natural := Index_Of (F, Name);
      N : Natural := 0;
   begin
      if I = 0 then
         return 0;
      end if;
      for K in 1 .. F.Series (I).Used loop
         if F.Series (I).Times (K) in From_Us .. To_Us then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Samples_Between;

   overriding procedure Find
     (F : in out Synthetic_Feed; Spec : Dash_Model.Series_Spec; Found : out Dash_Feeds.Series_Infos;
      Listed, Total : out Natural; Result : out Dash_Feeds.Status)
   is
      Wanted : constant String := Dash_Model.Image (Spec.Name);
   begin
      F.Call_Count := F.Call_Count + 1;
      Listed := 0;
      Total := 0;
      Found := [others => (others => <>)];
      for I in 1 .. F.Count loop
         declare
            Name : constant String := Dash_Model.Image (F.Series (I).Name);
            Matches : constant Boolean :=
              (if Spec.Match = Dash_Model.Exact then Name = Wanted
               else Name'Length >= Wanted'Length and then Name (Name'First .. Name'First + Wanted'Length - 1) = Wanted);
         begin
            if Matches and then not Refused (F, Name) then
               Total := Total + 1;
               if Listed < Found'Length then
                  Listed := Listed + 1;
                  Found (Found'First + Listed - 1) :=
                    (Id => Unsigned_64 (I), Name => F.Series (I).Name, Source => Dash_Model.To_Text ("desktop"),
                     Kind => F.Series (I).Kind, Unit => F.Series (I).Unit,
                     Last_Us => (if F.Series (I).Used > 0 then F.Series (I).Times (F.Series (I).Used) else 0),
                     Raw_From_Us => 0, Published => Unsigned_64 (F.Series (I).Used));
               end if;
            end if;
         end;
      end loop;
      Result := (if Refused (F, Wanted) then Dash_Feeds.Out_Of_Scope else Dash_Feeds.OK);
   end Find;

   --  The store's page: 12 list rows (CuBit.Metric_Query_Pages.List_Rows).
   List_Page : constant := 12;

   overriding procedure List_All
     (F : in out Synthetic_Feed; Skip : Natural; Found : out Dash_Feeds.Series_Infos; Listed, Total : out Natural;
      Result : out Dash_Feeds.Status) is
   begin
      F.Call_Count := F.Call_Count + 1;
      Found := [others => (others => <>)];
      Listed := 0;
      Total := 0;
      for I in 1 .. F.Count loop
         declare
            Name : constant String := Dash_Model.Image (F.Series (I).Name);
         begin
            if not Refused (F, Name) then
               Total := Total + 1;
               if Total > Skip and then Listed < Natural'Min (Found'Length, List_Page) then
                  Listed := Listed + 1;
                  Found (Found'First + Listed - 1) :=
                    (Id => Unsigned_64 (I), Name => F.Series (I).Name, Source => Dash_Model.To_Text ("desktop"),
                     Kind => F.Series (I).Kind, Unit => F.Series (I).Unit,
                     Last_Us => (if F.Series (I).Used > 0 then F.Series (I).Times (F.Series (I).Used) else 0),
                     Raw_From_Us => 0, Published => Unsigned_64 (F.Series (I).Used));
               end if;
            end if;
         end;
      end loop;
      Result := Dash_Feeds.OK;
   end List_All;

   function Valid (F : Synthetic_Feed; Id : Dash_Feeds.Series_Id) return Boolean is
     (Id in 1 .. Unsigned_64 (F.Count));

   overriding procedure Read_Raw
     (F : in out Synthetic_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip : Unsigned_64;
      Times, Values : out Charts_Quantiles.Sample_Array; Rows, Total : out Natural; Result : out Dash_Feeds.Status)
   is
      Seen : Unsigned_64 := 0;
      Page : constant Natural := Natural'Min (Times'Length, Raw_Page);
   begin
      F.Call_Count := F.Call_Count + 1;
      Rows := 0;
      Total := 0;
      Times := [others => 0];
      Values := [others => 0];
      if not Valid (F, Id) then
         Result := Dash_Feeds.Not_Found;
         return;
      end if;
      declare
         S : Series_Data renames F.Series (Natural (Id));
      begin
         for K in 1 .. S.Used loop
            if S.Times (K) in From_Us .. To_Us then
               if Seen >= Skip and then Rows < Page then
                  Times (Times'First + Rows) := S.Times (K);
                  Values (Values'First + Rows) := S.Values (K);
                  Rows := Rows + 1;
               end if;
               Seen := Seen + 1;
            end if;
         end loop;
      end;
      Total := Natural (Seen);
      Result := Dash_Feeds.OK;
   end Read_Raw;

   procedure Summarize (Values : in out Q.Sample_Array; Row : in out Charts_Columns.Window_Row);
   procedure Summarize (Values : in out Q.Sample_Array; Row : in out Charts_Columns.Window_Row) is
      N : constant Positive := Values'Length;
   begin
      Row.Count := Unsigned_64 (N);
      Row.Minimum := Unsigned_64'Last;
      for V of Values loop
         Row.Sum := Row.Sum + V;
         Row.Minimum := Unsigned_64'Min (Row.Minimum, V);
         Row.Maximum := Unsigned_64'Max (Row.Maximum, V);
      end loop;
      Q.Select_Kth (Values, Q.Rank (Q.P99, N), Row.P99);
      Q.Select_Kth (Values, Q.Rank (Q.Median, N), Row.P50);
   end Summarize;

   overriding procedure Read_Windows
     (F : in out Synthetic_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip, Step_Us : Unsigned_64;
      Windows : out Charts_Columns.Window_Rows; Rows, Total : out Natural; Resolution_Us : out Unsigned_64;
      Result : out Dash_Feeds.Status)
   is
      W : Unsigned_64 := Tier_Widths (1);
      Page : constant Natural := Natural'Min (Windows'Length, Window_Page);
      Seen : Unsigned_64 := 0;
   begin
      F.Call_Count := F.Call_Count + 1;
      for T of Tier_Widths loop
         if T <= Step_Us then
            W := T;
         end if;
      end loop;
      Resolution_Us := W;
      Rows := 0;
      Total := 0;
      Windows := [others => (others => <>)];
      if not Valid (F, Id) then
         Result := Dash_Feeds.Not_Found;
         return;
      end if;
      declare
         S : Series_Data renames F.Series (Natural (Id));
         Start : Unsigned_64 := From_Us / W * W;
         K : Natural := 1;
      begin
         while Start <= To_Us loop
            declare
               Buffer : Q.Sample_Array (1 .. S.Used + 1);
               N : Natural := 0;
               Row : Charts_Columns.Window_Row := (Start_Us => Start, others => <>);
            begin
               while K <= S.Used and then S.Times (K) < Start loop
                  K := K + 1;
               end loop;
               while K <= S.Used and then S.Times (K) < Start + W loop
                  N := N + 1;
                  Buffer (N) := S.Values (K);
                  Row.Last := S.Values (K);
                  K := K + 1;
               end loop;
               if N > 0 then
                  if Seen >= Skip and then Rows < Page then
                     Summarize (Buffer (1 .. N), Row);
                     Windows (Windows'First + Rows) := Row;
                     Rows := Rows + 1;
                  end if;
                  Seen := Seen + 1;
               end if;
               exit when K > S.Used;
            end;
            Start := Start + W;
         end loop;
      end;
      Total := Natural (Seen);
      Result := Dash_Feeds.OK;
   end Read_Windows;

   overriding procedure Read_Stats
     (F : in out Synthetic_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us : Unsigned_64;
      Stats : out Dash_Feeds.Window_Stats; Result : out Dash_Feeds.Status)
   is
   begin
      F.Call_Count := F.Call_Count + 1;
      Stats := (others => 0);
      if not Valid (F, Id) then
         Result := Dash_Feeds.Not_Found;
         return;
      end if;
      declare
         S : Series_Data renames F.Series (Natural (Id));
         Buffer : Q.Sample_Array (1 .. S.Used + 1);
         N : Natural := 0;
      begin
         Stats.Minimum := Unsigned_64'Last;
         for K in 1 .. S.Used loop
            if S.Times (K) in From_Us .. To_Us then
               N := N + 1;
               Buffer (N) := S.Values (K);
               Stats.Sum := Stats.Sum + S.Values (K);
               Stats.Minimum := Unsigned_64'Min (Stats.Minimum, S.Values (K));
               Stats.Maximum := Unsigned_64'Max (Stats.Maximum, S.Values (K));
               Stats.Last := S.Values (K);
            end if;
         end loop;
         Stats.Count := Unsigned_64 (N);
         if N > 0 then
            Q.Select_Kth (Buffer (1 .. N), Q.Rank (Q.Median, N), Stats.P50);
            Q.Select_Kth (Buffer (1 .. N), Q.Rank (Q.P90, N), Stats.P90);
            Q.Select_Kth (Buffer (1 .. N), Q.Rank (Q.P99, N), Stats.P99);
            Q.Select_Kth (Buffer (1 .. N), Q.Rank (999, N), Stats.P999);
         else
            Stats.Minimum := 0;
         end if;
         if To_Us > From_Us then
            Stats.Rate_Milli := Stats.Count * 1_000_000_000 / (To_Us - From_Us);
         end if;
      end;
      Result := Dash_Feeds.OK;
   end Read_Stats;

   overriding function Now_Us (F : Synthetic_Feed) return Unsigned_64 is (F.Clock);
   overriding function Calls (F : Synthetic_Feed) return Unsigned_64 is (F.Call_Count);
end Synthetic_Feeds;
