pragma Ada_2022;
with CuBit.Metric_Outlet;
with Metric_Sketches;

package body Metric_Reads is
   use CuBit.Metric_Protocol;
   package Series renames Metric_Series;
   package Sketches renames Metric_Sketches;
   package Outlet renames CuBit.Metric_Outlet;
   use type Series.Name_Match;
   use type Outlet.Series_Kind;
   use type Outlet.Unit;

   P50 : constant := 500;
   P90 : constant := 900;
   P99 : constant := 990;
   P999 : constant := 999;
   Milli : constant := 1_000;
   Microseconds_Per_Second : constant := 1_000_000;

   function Add (A, B : Unsigned_64) return Unsigned_64 is
     (if B > Unsigned_64'Last - A then Unsigned_64'Last else A + B);

   function Name_Of (Store : Series.Store; Index : Series.Series_Index) return String is
     (Series.Text_Of (Store, Series.Info (Store, Index).With_Labels.Name));

   --  The selector the reader wrote at the page's start.
   function Selector_Of (Item : Pages.Page) return Series.Selector is
      Sel : Series.Selector;
      Name : constant String := Pages.Get_Text (Item, Pages.Select_Name, Pages.Text_Bytes);
      Source : constant String := Pages.Get_Text (Item, Pages.Select_Source, Pages.Text_Bytes);
   begin
      Sel.Match := (case Item (Pages.Select_Match) is
                      when Series.Name_Match'Enum_Rep (Series.Exact_Name) => Series.Exact_Name,
                      when Series.Name_Match'Enum_Rep (Series.Name_Prefix) => Series.Name_Prefix,
                      when others => Series.Any_Name);
      Sel.Name (1 .. Name'Length) := Name;
      Sel.Name_Length := Name'Length;
      Sel.Source (1 .. Source'Length) := Source;
      Sel.Source_Length := Source'Length;
      return Sel;
   end Selector_Of;

   --  A reader with no scope at all, or an exact name outside its scopes.
   function Outside (Scopes : Metric_Scopes.Table; Reader : Unsigned_64; Sel : Series.Selector) return Boolean is
     (not Metric_Scopes.Has_Any (Scopes, Reader)
      or else (Sel.Match = Series.Exact_Name
               and then not Metric_Scopes.May_Read (Scopes, Reader, Sel.Name (1 .. Sel.Name_Length))));

   procedure List
     (Store : Metric_Series.Store; Scopes : Metric_Scopes.Table; Reader : Unsigned_64;
      Skip : Unsigned_64; Item : in out Pages.Page; Rows, Total : out Unsigned_64;
      Result : out CuBit.Metric_Protocol.Status)
   is
      Sel : constant Series.Selector := Selector_Of (Item);
      At_Series : Series.Series_Count := Series.No_Series;
      Base : Natural;
   begin
      Rows := 0;
      Total := 0;
      Item := [others => 0];
      if Outside (Scopes, Reader, Sel) then
         Result := Out_Of_Scope;
         return;
      end if;
      loop
         Series.Next_Selected (Store, Sel, At_Series, At_Series);
         exit when At_Series = Series.No_Series;
         if Metric_Scopes.May_Read (Scopes, Reader, Name_Of (Store, At_Series)) then
            if Total >= Skip and then Rows < Pages.List_Rows then
               declare
                  I : constant Series.Series_Info := Series.Info (Store, At_Series);
               begin
                  Base := Natural (Rows) * Pages.List_Row_Words;
                  Item (Base + Pages.Row_Id) := I.Id;
                  Item (Base + Pages.Row_Kind) := Unsigned_64 (Outlet.Series_Kind'Enum_Rep (I.Kind));
                  Item (Base + Pages.Row_Unit) := Unsigned_64 (Outlet.Unit'Enum_Rep (I.Measure));
                  Item (Base + Pages.Row_Published) := I.Published;
                  Item (Base + Pages.Row_Retained) := I.Retained;
                  Item (Base + Pages.Row_Evicted) := I.Evicted;
                  Item (Base + Pages.Row_Shed) := I.Shed;
                  Item (Base + Pages.Row_Refused) := I.Refused;
                  Item (Base + Pages.Row_First) := I.First_Us;
                  Item (Base + Pages.Row_Last) := I.Last_Us;
                  Item (Base + Pages.Row_Last_Value) := I.Last_Value;
                  Item (Base + Pages.Row_Raw_From) := I.Raw_From_Us;
                  Item (Base + Pages.Row_Gaps) := Unsigned_64 (I.Gaps);
                  Item (Base + Pages.Row_Raw_Bytes) := I.Raw_Bytes;
                  Pages.Put_Text (Item, Base + Pages.Row_Name, Series.Text_Of (Store, I.With_Labels.Name),
                                  Pages.Text_Bytes);
                  Pages.Put_Text (Item, Base + Pages.Row_Source, Series.Text_Of (Store, I.With_Labels.Source),
                                  Pages.Text_Bytes);
                  Pages.Put_Text (Item, Base + Pages.Row_Program, Series.Text_Of (Store, I.With_Labels.Program),
                                  Pages.Short_Text_Bytes);
                  Pages.Put_Text (Item, Base + Pages.Row_Instance, Series.Text_Of (Store, I.With_Labels.Instance),
                                  Pages.Short_Text_Bytes);
               end;
               Rows := Rows + 1;
            end if;
            Total := Total + 1;
         end if;
      end loop;
      Result := OK;
   end List;

   procedure Read_Range
     (Store : Metric_Series.Store; Scopes : Metric_Scopes.Table; Reader : Unsigned_64;
      Id, From_Us, To_Us : Unsigned_64; Item : in out Pages.Page;
      Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Result : out CuBit.Metric_Protocol.Status)
   is
      Index : constant Series.Series_Count :=
        (if Id <= Series.Id_Mask then Series.Find_Id (Store, Id) else Series.No_Series);
      Skip : constant Unsigned_64 := Item (Pages.Range_Skip);
      Step : constant Unsigned_64 := Item (Pages.Range_Step);
      Seen : Unsigned_64 := 0;
      procedure Raw_Row (Time_Us, Value : Unsigned_64; Continue : out Boolean);
      procedure Raw_Row (Time_Us, Value : Unsigned_64; Continue : out Boolean) is
      begin
         Continue := True;
         if Seen >= Skip and then Rows < Pages.Raw_Rows then
            Item (Pages.Range_Rows_At + Natural (Rows) * Pages.Raw_Row_Words) := Time_Us;
            Item (Pages.Range_Rows_At + Natural (Rows) * Pages.Raw_Row_Words + 1) := Value;
            Rows := Rows + 1;
         end if;
         Seen := Seen + 1;
      end Raw_Row;
      procedure Raw is new Series.Read_Raw (Raw_Row);
      procedure Window_Row (Row : Series.Window_Row; Continue : out Boolean);
      procedure Window_Row (Row : Series.Window_Row; Continue : out Boolean) is
         Base : constant Natural := Pages.Range_Rows_At + Natural (Rows) * Pages.Window_Row_Words;
      begin
         Continue := True;
         if Seen >= Skip and then Rows < Pages.Window_Rows then
            Item (Base) := Row.Start_Us;
            Item (Base + 1) := Row.Count;
            Item (Base + 2) := Row.Sum;
            Item (Base + 3) := Row.Minimum;
            Item (Base + 4) := Row.Maximum;
            Item (Base + 5) := Row.Last;
            Item (Base + 6) := Row.P50;
            Item (Base + 7) := Row.P99;
            Rows := Rows + 1;
         end if;
         Seen := Seen + 1;
      end Window_Row;
      procedure Windows is new Series.Read_Windows (Window_Row);
      procedure Gap_Row (G : Series.Gap; Continue : out Boolean);
      procedure Gap_Row (G : Series.Gap; Continue : out Boolean) is
         Base : constant Natural := Pages.Range_Gaps_At + Natural (Gaps) * Pages.Gap_Row_Words;
      begin
         Continue := Gaps + 1 < Pages.Range_Gap_Rows;
         Item (Base) := G.First_Us;
         Item (Base + 1) := G.Last_Us;
         Item (Base + 2) := G.Count;
         Item (Base + 3) := Unsigned_64 (Series.Gap_Reason'Enum_Rep (G.Reason));
         Gaps := Gaps + 1;
      end Gap_Row;
      procedure Read_Gaps is new Series.Read_Gaps (Gap_Row);
   begin
      Rows := 0;
      Total := 0;
      Gaps := 0;
      Resolution_Us := 0;
      Item := [others => 0];
      if Index = Series.No_Series then
         Result := Not_Found;
         return;
      end if;
      if not Metric_Scopes.May_Read (Scopes, Reader, Name_Of (Store, Index)) then
         Result := Out_Of_Scope;
         return;
      end if;
      Item (Pages.Range_Raw_From) := Series.Info (Store, Index).Raw_From_Us;
      Read_Gaps (Store, Index, From_Us, To_Us);
      if Step = 0 then
         Raw (Store, Index, From_Us, To_Us);
      else
         Resolution_Us := Series.Width_Us (Series.Tier_For_Step (Step));
         Windows (Store, Index, Series.Tier_For_Step (Step), From_Us, To_Us);
      end if;
      Total := Seen;
      Result := OK;
   end Read_Range;

   procedure Read_Window
     (Store : Metric_Series.Store; Scopes : Metric_Scopes.Table; Reader : Unsigned_64;
      Id, From_Us, To_Us : Unsigned_64; Item : in out Pages.Page;
      Merged : out Unsigned_64; Result : out CuBit.Metric_Protocol.Status)
   is
      Sel : constant Series.Selector := Selector_Of (Item);
      D : Sketches.Dense;
      Resolution, Missing, Coarsest, Lost : Unsigned_64 := 0;
      Kind : Outlet.Series_Kind := Outlet.Counter;
      Measure : Outlet.Unit := Outlet.Count;
      Last, Last_Time : Unsigned_64 := 0;
      At_Series : Series.Series_Count := Series.No_Series;
      procedure Fold (Index : Series.Series_Index);
      procedure Fold (Index : Series.Series_Index) is
         I : constant Series.Series_Info := Series.Info (Store, Index);
      begin
         if Merged > 0 and then (I.Kind /= Kind or else I.Measure /= Measure) then
            --  Only like series merge: the first's kind and unit decide.
            return;
         end if;
         Kind := I.Kind;
         Measure := I.Measure;
         Series.Summarize (Store, Index, From_Us, To_Us, D, Resolution, Missing);
         Coarsest := Unsigned_64'Max (Coarsest, Resolution);
         Lost := Add (Lost, Missing);
         if I.Last_Us >= Last_Time and then I.Last_Us <= To_Us then
            Last_Time := I.Last_Us;
            Last := I.Last_Value;
         end if;
         Merged := Merged + 1;
      end Fold;
      Seconds_Milli : Unsigned_64;
   begin
      Merged := 0;
      if Id /= 0 then
         declare
            Index : constant Series.Series_Count :=
              (if Id <= Series.Id_Mask then Series.Find_Id (Store, Id) else Series.No_Series);
         begin
            Item := [others => 0];
            if Index = Series.No_Series then
               Result := Not_Found;
               return;
            end if;
            if not Metric_Scopes.May_Read (Scopes, Reader, Name_Of (Store, Index)) then
               Result := Out_Of_Scope;
               return;
            end if;
            Fold (Index);
         end;
      else
         Item := [others => 0];
         if Outside (Scopes, Reader, Sel) then
            Result := Out_Of_Scope;
            return;
         end if;
         loop
            Series.Next_Selected (Store, Sel, At_Series, At_Series);
            exit when At_Series = Series.No_Series;
            if Metric_Scopes.May_Read (Scopes, Reader, Name_Of (Store, At_Series)) then
               Fold (At_Series);
            end if;
         end loop;
      end if;
      if Merged = 0 then
         Result := Not_Found;
         return;
      end if;
      Item (Pages.Stat_Count) := D.Count;
      Item (Pages.Stat_Sum) := D.Sum;
      Item (Pages.Stat_Minimum) := (if D.Count = 0 then 0 else D.Minimum);
      Item (Pages.Stat_Maximum) := D.Maximum;
      Item (Pages.Stat_Last) := Last;
      Item (Pages.Stat_P50) := Sketches.Quantile (D, P50);
      Item (Pages.Stat_P90) := Sketches.Quantile (D, P90);
      Item (Pages.Stat_P99) := Sketches.Quantile (D, P99);
      Item (Pages.Stat_P999) := Sketches.Quantile (D, P999);
      --  Per second over the window asked for: a counter's increments,
      --  anything else's samples.
      Seconds_Milli := (if To_Us > From_Us then (To_Us - From_Us) / Milli else 0);
      Item (Pages.Stat_Rate_Milli) :=
        (if Seconds_Milli = 0 then 0
         elsif Kind = Outlet.Counter then
           (if D.Sum > Unsigned_64'Last / Microseconds_Per_Second then Unsigned_64'Last
            else D.Sum * Microseconds_Per_Second / Seconds_Milli)
         else (if D.Count > Unsigned_64'Last / Microseconds_Per_Second then Unsigned_64'Last
               else D.Count * Microseconds_Per_Second / Seconds_Milli));
      Item (Pages.Stat_Missing) := Lost;
      Item (Pages.Stat_Resolution) := Coarsest;
      Item (Pages.Stat_Kind) := Unsigned_64 (Outlet.Series_Kind'Enum_Rep (Kind));
      Item (Pages.Stat_Unit) := Unsigned_64 (Outlet.Unit'Enum_Rep (Measure));
      Item (Pages.Stat_Error_Permille) := Sketches.Relative_Error_Permille;
      Item (Pages.Stat_Sum_Saturated) := (if D.Sum_Saturated then 1 else 0);
      Result := OK;
   end Read_Window;
end Metric_Reads;
