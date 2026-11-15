pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Deadlines;
with CuBit.Futex_Locks;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Thread_Areas;
with Charts_Format;
with Dash_Feeds;
with Dash_Files;
with Dash_Model;
with Dash_Sync;

package body Dash_Runner is
   package ABI renames CuBit.Kernel_ABI;
   use type Dash_Feeds.Status;

   --  PROTECT_OWNED_MEMORY: no access (the stack's guard page).
   Inaccessible : constant := 0;
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
   Input_Name : aliased constant String := "metricsview.input_to_render_us";
   Cost_Name : aliased constant String := "metricsview.input_cost_us";
   Update_Name : aliased constant String := "metricsview.update_us";
   Data_Name : aliased constant String := "metricsview.data_us";
   Render_Name : aliased constant String := "metricsview.render_us";
   Handle_Name : aliased constant String := "metricsview.handle_us";
   Wait_Name : aliased constant String := "metricsview.lock_wait_us";
   Own_Names : constant array (1 .. 7) of Name_Access :=
     [Input_Name'Access, Cost_Name'Access, Update_Name'Access, Data_Name'Access, Render_Name'Access,
      Handle_Name'Access, Wait_Name'Access];

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

   --  An edited dashboard, written when it is due (Dash_View.Take_Save):
   --  the value is copied under the lock and written without it.
   Save_Deadline_Ms : constant := 2_000;

   procedure Save_Step is
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
         Source : constant String := Dash_View.Dashboard_Source (View) & ASCII.LF;
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
      Stack := CuBit.Kernel_Calls.Call (ABI.Allocate_Owned_Memory, Data_Stack_Bytes);
      if Stack = 0 or else Stack = ABI.Failed then
         return;
      end if;
      --  A guard page at the bottom: an overflow faults instead of writing
      --  over a neighbour.
      if CuBit.Kernel_Calls.Call (ABI.Protect_Owned_Memory, Stack, ABI.Page_Bytes, Inaccessible) /= 0 then
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
