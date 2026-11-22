pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Deadlines;
with CuBit.Futex_Locks;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Owned_Reservations;
with CuBit.Thread_Areas;
with Charts_Format;
with Dash_Feeds;
with CuBit.File_Selection;
with Dash_Files;
with Dash_Jobs;
with Dash_Model;
with Dash_Sync;

package body Dash_Runner is
   package ABI renames CuBit.Kernel_ABI;
   use type Dash_Feeds.Status;

   --  A SysV function expects RSP + 8 to be 16-byte aligned at entry.
   Stack_Alignment : constant := 16;
   Return_Address_Bytes : constant := 8;
   --  The data thread reads its own series back this often (headless markers).
   Own_Report_Ms : constant := 2_000;

   type Word is mod 2 ** 32 with Atomic, Size => 32;
   --  Cleared and futex-woken by the kernel when the thread ends.
   Exit_Word : aliased Word := 1;
   Wake_Event : CuBit.Futex_Locks.Event;
   Stopping : Boolean := False with Atomic;
   Is_Running : Boolean := False with Atomic;
   Step_Count, Last_Us, Slowest_Us : Unsigned_64 := 0 with Atomic;

   Data_Cost : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.data_us", CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);

   procedure Observe (Item : CuBit.Metrics.Series; Value : Unsigned_64) is
   begin
      Dash_Sync.Lock;
      CuBit.Metrics.Observe (Item, Value);
      Dash_Sync.Unlock;
   end Observe;

   function Steps return Unsigned_64 is (Step_Count);
   function Last_Step_Us return Unsigned_64 is (Last_Us);
   function Slowest_Step_Us return Unsigned_64 is (Slowest_Us);
   function Running return Boolean is (Is_Running);

   procedure Wake is
   begin
      CuBit.Futex_Locks.Signal (Wake_Event);
   end Wake;

   procedure Stop is
   begin
      Stopping := True;
      Wake;
   end Stop;

   --  The headless markers' read-back of this app's own series (store
   --  calls: here, never on the UI thread).
   type Name_Access is access constant String;
   Name_1 : aliased constant String := "metricsview.input_to_render_us";
   Name_2 : aliased constant String := "metricsview.input_cost_us";
   Name_3 : aliased constant String := "metricsview.update_us";
   Name_4 : aliased constant String := "metricsview.data_us";
   Name_5 : aliased constant String := "metricsview.render_us";
   Name_6 : aliased constant String := "metricsview.handle_us";
   Name_7 : aliased constant String := "metricsview.lock_wait_us";
   Name_8 : aliased constant String := "metricsview.input_cost_edit_us";
   Name_9 : aliased constant String := "metricsview.input_to_render_edit_us";
   Name_10 : aliased constant String := "metricsview.input_cost_zoom_us";
   Name_11 : aliased constant String := "metricsview.input_to_render_zoom_us";
   Name_12 : aliased constant String := "metricsview.input_cost_hover_us";
   Name_13 : aliased constant String := "metricsview.input_to_render_hover_us";
   Name_14 : aliased constant String := "metricsview.input_cost_pan_us";
   Name_15 : aliased constant String := "metricsview.input_to_render_pan_us";
   Name_16 : aliased constant String := "metricsview.input_cost_other_us";
   Name_17 : aliased constant String := "metricsview.input_to_render_other_us";
   Name_18 : aliased constant String := "metricsview.copy_us";
   Name_19 : aliased constant String := "metricsview.copy_union_px";
   Name_20 : aliased constant String := "metricsview.copy_exact_px";
   Name_21 : aliased constant String := "metricsview.paint_us";
   Name_22 : aliased constant String := "metricsview.input_cost_menu_us";
   Name_23 : aliased constant String := "metricsview.input_to_render_menu_us";
   Name_24 : aliased constant String := "metricsview.input_cost_picker_us";
   Name_25 : aliased constant String := "metricsview.input_to_render_picker_us";
   Own_Names : constant array (Positive range <>) of Name_Access :=
     [Name_1'Access, Name_2'Access, Name_3'Access, Name_4'Access, Name_5'Access, Name_6'Access,
      Name_7'Access, Name_8'Access, Name_9'Access, Name_10'Access, Name_11'Access, Name_12'Access,
      Name_13'Access, Name_14'Access, Name_15'Access, Name_16'Access, Name_17'Access, Name_18'Access,
      Name_19'Access, Name_20'Access, Name_21'Access, Name_22'Access, Name_23'Access, Name_24'Access,
      Name_25'Access];

   procedure Report_Own;
   procedure Report_Own is
      Own : Dash_Feeds.Series_Infos (1 .. 1);
      Listed, Total : Natural;
      Result : Dash_Feeds.Status;
      Stats : Dash_Feeds.Window_Stats;
      function Decimal (V : Unsigned_64) return String renames Charts_Format.Decimal;
   begin
      for Name of Own_Names loop
         Dash_Query_Feeds.Find (Feed, (Name => Dash_Model.To_Text (Name.all), others => <>), Own, Listed, Total,
                                Result);
         if Result = Dash_Feeds.OK and then Listed = 1 then
            Dash_Query_Feeds.Read_Stats (Feed, Own (1).Id, 0, CuBit.Monotonic.Microseconds, Stats, Result);
            if Result = Dash_Feeds.OK then
               CuBit.Messages.debugPrint
                 ("metricsview: own series " & Name.all & " n=" & Decimal (Stats.Count)
                  & " p50_us=" & Decimal (Stats.P50) & " p99_us=" & Decimal (Stats.P99)
                  & " max_us=" & Decimal (Stats.Maximum) & ASCII.LF);
            end if;
         end if;
      end loop;
   end Report_Own;

   --  The dashboards folder's work (Dash_Jobs): Dash_Files in the
   --  dashboards' place, each call waiting at most File_Deadline_Ms.
   File_Deadline_Ms : constant := 2_000;
   Folder : constant String :=
     Dash_Model.Dashboards_Place (Dash_Model.Dashboards_Place'First .. Dash_Model.Dashboards_Place'Last - 1);

   function Result_Of (Result : Dash_Files.Outcome) return Dash_Jobs.File_Result is
     (case Result is
        when Dash_Files.Done => Dash_Jobs.File_Done,
        when Dash_Files.Not_Found => Dash_Jobs.File_Missing,
        when Dash_Files.Not_Granted => Dash_Jobs.File_Refused,
        when Dash_Files.Too_Large => Dash_Jobs.File_Too_Large,
        when Dash_Files.Unavailable => Dash_Jobs.File_Unavailable,
        when Dash_Files.Late => Dash_Jobs.File_Late,
        when Dash_Files.Failed => Dash_Jobs.File_Failed);
   function Deadline return Unsigned_64 is (CuBit.Monotonic.Milliseconds + File_Deadline_Ms);

   procedure List_Folder (Into : out Dash_View.Folder_Listing);
   procedure List_Folder (Into : out Dash_View.Folder_Listing) is
      Result : Dash_Files.Outcome;
      Listing : Dash_View.Folder_Listing;
      procedure Each (Name : String; Modified : Unsigned_64) is
         Accepted : Boolean;
         Suffix : constant String := Dash_Model.Dashboard_Suffix;
      begin
         --  Dashboards only (a save's hidden sibling is not one).
         if Name'Length > Suffix'Length and then Name (Name'Last - Suffix'Length + 1 .. Name'Last) = Suffix
           and then Name (Name'First) /= '.' and then Listing.Count < CuBit.File_Selection.Maximum_Files
         then
            CuBit.File_Selection.Set (Listing.Entries (Listing.Count + 1).Leaf, Name, Accepted);
            if Accepted then
               Listing.Count := Listing.Count + 1;
               Listing.Entries (Listing.Count).Modified := Modified;
            end if;
         end if;
      end Each;
      procedure Walk is new Dash_Files.List (Each);
      use type Dash_Files.Outcome;
   begin
      if not Dash_Files.Is_Open then
         Into := (others => <>);
         return;
      end if;
      Walk (Folder, Deadline, Result);
      --  No folder yet: an empty one (made at the first save).
      Listing.Ready := Result in Dash_Files.Done | Dash_Files.Not_Found;
      Into := Listing;
   end List_Folder;

   procedure Read_File (Leaf : String; Into : out String; Got : out Natural; Result : out Dash_Jobs.File_Result);
   procedure Read_File (Leaf : String; Into : out String; Got : out Natural; Result : out Dash_Jobs.File_Result) is
      Outcome : Dash_Files.Outcome;
   begin
      Dash_Files.Load (Dash_Model.Dashboards_Place & Leaf, Into, Got, Deadline, Outcome);
      Result := Result_Of (Outcome);
   end Read_File;

   procedure Write_File (Leaf, Bytes : String; Result : out Dash_Jobs.File_Result);
   procedure Write_File (Leaf, Bytes : String; Result : out Dash_Jobs.File_Result) is
      Outcome : Dash_Files.Outcome;
   begin
      Dash_Files.Save (Dash_Model.Dashboards_Place & Leaf, Bytes, Deadline, Outcome);
      Result := Result_Of (Outcome);
      CuBit.Messages.debugPrint
        ("metricsview: saved as " & Leaf & " bytes=" & Charts_Format.Decimal (Unsigned_64 (Bytes'Length))
         & " result=" & Outcome'Image & ASCII.LF);
   end Write_File;

   function File_Exists (Leaf : String) return Boolean;
   function File_Exists (Leaf : String) return Boolean is
      Found : Boolean;
      Outcome : Dash_Files.Outcome;
   begin
      Dash_Files.Exists (Dash_Model.Dashboards_Place & Leaf, Deadline, Found, Outcome);
      return Found;
   end File_Exists;

   package Jobs is new Dash_Jobs.Worker (List_Folder, Read_File, Write_File, File_Exists);

   --  An edited dashboard, written when it is due (Dash_View.Take_Save):
   --  the value is copied under the lock and written without it.
   Save_Deadline_Ms : constant := 2_000;

   procedure Autosave;
   procedure Autosave is
      Due : Boolean;
      Generation : Natural;
   begin
      Dash_Sync.Lock;
      Dash_View.Take_Save (View, Due, Generation);
      if not Due then
         Dash_Sync.Unlock;
         return;
      end if;
      declare
         Source : constant String := Dash_View.Save_Source (View) & ASCII.LF;
         Name : constant String := Dash_View.Save_Name (View);
         Result : Dash_Files.Outcome := Dash_Files.Unavailable;
         use type Dash_Files.Outcome;
      begin
         Dash_Sync.Unlock;
         if Dash_Files.Is_Open then
            Dash_Files.Save (Name, Source, CuBit.Monotonic.Milliseconds + Save_Deadline_Ms, Result);
         end if;
         Dash_Sync.Lock;
         Dash_View.End_Save
           (View, Generation, Result = Dash_Files.Done,
            (case Result is
               when Dash_Files.Done => "",
               when Dash_Files.Unavailable | Dash_Files.Not_Granted =>
                 "Can't save: this app has no folder to save dashboards in",
               when Dash_Files.Not_Found => "Can't save: the Dashboards folder could not be made",
               when Dash_Files.Late => "Can't save: the disk did not answer",
               when Dash_Files.Too_Large | Dash_Files.Failed =>
                 "Can't save: the disk refused it (error "
                 & Charts_Format.Decimal (Unsigned_64 (Dash_Files.Last_Reply)) & ")"));
         Dash_Sync.Unlock;
         CuBit.Messages.debugPrint
           ("metricsview: " & (if Result = Dash_Files.Done then "saved " else "not saved ") & Name & " bytes="
            & Charts_Format.Decimal (Unsigned_64 (Source'Length)) & " result=" & Result'Image & ASCII.LF);
      end;
   end Autosave;

   procedure Save_Step is
   begin
      Autosave;
      Jobs.Step (View);
   end Save_Step;

   procedure Data_Loop (Argument : Unsigned_64) with Convention => C, No_Return;
   procedure Data_Loop (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Own_Due_Ms : Unsigned_64 := CuBit.Monotonic.Milliseconds + Own_Report_Ms;
   begin
      Is_Running := True;
      loop
         declare
            Seen : constant Unsigned_32 := CuBit.Futex_Locks.Current (Wake_Event);
            Ignore : constant Boolean :=
              CuBit.Futex_Locks.Wait
                (Wake_Event, Seen, CuBit.Deadlines.At_Monotonic (CuBit.Monotonic.Milliseconds + Data_Poll_Ms));
         begin
            null;
         end;
         exit when Stopping;
         declare
            Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
            Taken : Unsigned_64;
         begin
            Dash_View.Data_Step (View);
            Taken := CuBit.Monotonic.Microseconds - Started;
            Last_Us := Taken;
            Slowest_Us := Unsigned_64'Max (Slowest_Us, Taken);
            Step_Count := Step_Count + 1;
            Observe (Data_Cost, Taken);
         end;
         Save_Step;
         if CuBit.Monotonic.Milliseconds >= Own_Due_Ms then
            Own_Due_Ms := CuBit.Monotonic.Milliseconds + Own_Report_Ms;
            Report_Own;
         end if;
      end loop;
      Is_Running := False;
      CuBit.Thread_Areas.Exit_Thread;
   end Data_Loop;

   procedure Start (Started : out Boolean) is
      Stack, Area : Unsigned_64;
   begin
      Started := False;
      Stack := CuBit.Owned_Reservations.Reserve (Data_Stack_Bytes);
      if Stack = 0 then
         return;
      end if;
      --  A guard page at the bottom: it is never committed, so an overflow
      --  faults instead of writing over a neighbour.
      if not CuBit.Owned_Reservations.Commit_Steps
           (Stack + ABI.Page_Bytes, Data_Stack_Bytes - ABI.Page_Bytes)
      then
         return;
      end if;
      Area := CuBit.Thread_Areas.Create;
      if Area = CuBit.Thread_Areas.No_Area then
         return;
      end if;
      if CuBit.Kernel_Calls.Call
           (ABI.Thread_Create, Unsigned_64 (To_Integer (Data_Loop'Address)),
            ((Stack + Data_Stack_Bytes) and not (Stack_Alignment - 1)) - Return_Address_Bytes,
            0, Area, Unsigned_64 (To_Integer (Exit_Word'Address))) = ABI.Failed
      then
         CuBit.Thread_Areas.Discard (Area);
         return;
      end if;
      Started := True;
   end Start;
end Dash_Runner;
