------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  boot-timeline (BOOT-001, docs/boot-timeline.md): reads the kernel's boot
--  timeline, the granted publication its manifest declares
--  ("boot-timeline", mapped read-only), once the Desktop has presented its
--  first frame, and prints it: one line per stage with its
--  offset from power-on and the delta, every long startup wait ranked, the
--  per-process spans, the critical path, the first-frame budget (reported,
--  not enforced) and whether the record is complete.
--
--  Lines start with "BT-" so serial logs and the Logs viewer can be
--  filtered; the verdict is "BOOT-TIMELINE: COMPLETE" or "...: INCOMPLETE".
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with CuBit.Waits;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Block;
pragma Elaborate_All (CuBit.Launch_Block);
with CuBit.Publication_Grants;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
with System_Publications;
with Boot_Timeline_Layout; use Boot_Timeline_Layout;
with Boot_Timeline_Report; use Boot_Timeline_Report;

procedure Main is
   use ASCII;
   package SP renames System_Publications;

   --  How long to wait for the first frame, how often to look, and how long
   --  to let late services reach their request loops before reading.
   Goal_Wait_Ms    : constant := 60_000;
   Poll_Ms         : constant := 250;
   Settle_Ms       : constant := 3_000;
   --  BOOT-001's target: power-on to a usable desktop.
   Budget_Ms       : constant := 2_000;
   --  Ranked lines printed per table.
   Ranked_Lines    : constant := 25;

   --  The published Image, and the snapshot the report reads (the kernel
   --  rewrites the publication while its window is open).
   type Image_Bytes_Array is array (1 .. Image_Bytes) of Unsigned_8;
   Published : Image_Bytes_Array
     with Import, Volatile,
          Address => System.Storage_Elements.To_Address (SP.Boot_Timeline_Address);
   Buffer : Image_Bytes_Array := [others => 0] with Alignment => 8;
   Img : Image with Import, Address => Buffer'Address;

   Frequency : Counter_Frequency := Minimum_Frequency;

   procedure Put (Text : String) is
   begin
      debugPrint (Text);
   end Put;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Pad (Text : String; Width : Positive) return String is
     (if Text'Length >= Width then Text else (1 .. Width - Text'Length => ' ') & Text);

   --  Microseconds as milliseconds with three decimals.
   function Ms (Us : Microseconds) return String is
      Fraction : constant String := Decimal (1_000 + Us mod 1_000);
   begin
      return Decimal (Us / 1_000) & "." & Fraction (Fraction'First + 1 .. Fraction'Last);
   end Ms;

   function Us_Of (T : Ticks) return Microseconds is (To_Microseconds (T, Frequency));

   function Trim (Name : Name_Text) return String is
      Last : Natural := Name'Last;
   begin
      while Last >= Name'First and then Name (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      return Name (Name'First .. Last);
   end Trim;

   --  The user runtime discards enumeration names: spell them here.
   function Stage_Name (Stage : Kernel_Stage) return String is
     (case Stage is
        when Kernel_Entry             => "entry (firmware and loader done)",
        when Memory_Ready             => "memory ready",
        when Framebuffer_Ready        => "framebuffer ready",
        when Firmware_Tables_Ready    => "ACPI tables read",
        when Firmware_Timers_Owned    => "firmware timers owned",
        when Clock_Calibrated         => "clock calibrated",
        when Scheduling_Clock_Started => "scheduling clock started",
        when Interrupt_Routing_Ready  => "interrupt routing ready",
        when Initrd_Parsed            => "initrd parsed",
        when Device_Manager_Loaded    => "devmgr loaded",
        when Secondary_CPUs_Ready     => "secondary CPUs ready",
        when Scheduler_Started        => "scheduler started",
        when Device_Manager_Roots_Refused => "devmgr refused: ROOT_INVENTORY_CAPACITY");

   function Milestone_Name (Item : Milestone) return String is
     (case Item is
        when Startup_Profile_Started  => "startup profile started",
        when Startup_Profile_Launched => "startup profile launched",
        when Display_Leased           => "display leased",
        when First_Frame_Presented    => "FIRST FRAME PRESENTED",
        when First_Input_Accepted     => "FIRST INPUT ACCEPTED");

   function Name_Of (N : Process_Count) return String is
     (if N = 0 then "kernel" else Trim (Img.Processes (N).Name));

   function Who (Identity : Identity_Word) return String is
      N : constant Process_Count := Index_Of (Img, Identity);
   begin
      if Identity = No_Identity then
         return "-";
      elsif N = 0 then
         return "pid " & Decimal (Identity and 16#FFFF_FFFF#);
      end if;
      return Name_Of (N);
   end Who;

   function Hex (Value : Unsigned_32) return String is
      Digits_Text : constant String := "0123456789ABCDEF";
      Result : String (1 .. 4);
      V : Unsigned_32 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digits_Text (Natural (V and 16#F#) + 1);
         V := Shift_Right (V, 4);
      end loop;
      return "0x" & Result;
   end Hex;

   function Wait_Text (W : Wait_Number) return String is
      Item : Wait_Entry renames Img.Waits (W);
      Times : constant String :=
        (if Item.Repeats > 1 then " x" & Decimal (Unsigned_64 (Item.Repeats)) else "");
   begin
      case Item.Kind is
         when Call =>
            return "SYNC call " & Who (Item.Waiter) & " -> " & Who (Item.Target) &
              " label " & Hex (Item.Label) & Times;
         when Sleep => return "sleep in " & Who (Item.Waiter) & Times;
         when Receive => return "startup receive in " & Who (Item.Waiter) & Times;
         when Completion => return "startup completion wait in " & Who (Item.Waiter) & Times;
         when Futex => return "startup futex wait in " & Who (Item.Waiter) & Times;
         when No_Wait => return "?";
      end case;
   end Wait_Text;

   ---------------------------------------------------------------------------
   --  Reading the publication
   ---------------------------------------------------------------------------
   type Read_Result is (Read_OK, Not_Granted);

   --  Whether the startup table holds the publication at its address: the
   --  kernel mapped it only then (any other read of the address faults).
   function Granted return Boolean is
     (CuBit.Launch_Block.Table_Present and then CuBit.Startup_State.Granted
        (CuBit.Publication_Grants.Boot_Timeline_Name, CuBit.Startup_Tables.Publication)
      = SP.Boot_Timeline_Address);

   --  Snapshot the Image into Buffer.
   function Read_Image return Read_Result is
   begin
      if not Granted then
         return Not_Granted;
      end if;
      Buffer := Published;
      return Read_OK;
   end Read_Image;

   --  Scalars arrive as bytes: check each before the report reads them.
   function Valid_Image return Boolean is
   begin
      if Img.Head.Version /= Layout_Version or else not Img.Head.Recording'Valid
        or else not Img.Head.Processes'Valid or else not Img.Head.Waits'Valid
        or else not Img.Head.Processes_Dropped'Valid or else not Img.Head.Waits_Dropped'Valid
      then
         return False;
      end if;
      for N in 1 .. Img.Head.Processes loop
         if not Img.Processes (N).Calls'Valid or else not Img.Processes (N).Waits'Valid then
            return False;
         end if;
      end loop;
      for W in 1 .. Img.Head.Waits loop
         if not Img.Waits (W).Kind'Valid or else Img.Waits (W).Kind = No_Wait then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Image;

   function Goal_Reached return Boolean is
     (Img.Head.Version = Layout_Version and then
      Img.Milestones (Slot_Of (First_Frame_Presented)).At_Ticks /= Not_Recorded);

   ---------------------------------------------------------------------------
   --  Printing
   ---------------------------------------------------------------------------
   procedure Print_Timeline is
      Events : Event_List;
      Count : Listed_Count;
      Previous : Ticks := 0;
   begin
      Collect (Img, Events, Count);
      Put ("BT-TIMELINE: offset from power-on (ms), delta (ms), event" & LF);
      for I in 1 .. Count loop
         declare
            E : Event renames Events (I);
            What : constant String :=
              (case E.Kind is
                 when Stage_Reached => "kernel " & Stage_Name (E.Stage),
                 when Spawned =>
                   "spawn " & Name_Of (E.Process) & " by " & Name_Of (Index_Of (Img, Img.Processes (E.Process).Spawner)) &
                   " (ELF load " &
                   Ms (Us_Of (Img.Processes (E.Process).Loaded - Img.Processes (E.Process).Spawn_Started)) &
                   " ms)",
                 when Resumed => "resume " & Name_Of (E.Process),
                 when First_Run => "first instruction " & Name_Of (E.Process),
                 when Registered => "registered " & Name_Of (E.Process),
                 when Ready =>
                   "ready " & Name_Of (E.Process) &
                   (if Img.Processes (E.Process).Ready = Not_Recorded
                    then " (first receive; never registered)" else " (request loop)"),
                 when Milestone_Marked =>
                   "MILESTONE " & Milestone_Name (E.Item) & " by " &
                   Who (Img.Milestones (Slot_Of (E.Item)).Marker));
         begin
            Put ("BT-LINE " & Pad (Ms (Us_Of (E.At_Ticks)), 11) & " " &
                 Pad ("+" & Ms (Us_Of (E.At_Ticks - Previous)), 11) & "  " & What & LF);
            Previous := E.At_Ticks;
         end;
      end loop;
   end Print_Timeline;

   procedure Print_Waits is
      Order : array (1 .. Wait_Capacity) of Wait_Count := [others => 0];
      Used : Natural := 0;
   begin
      --  Rank by duration (insertion sort; at most Wait_Capacity entries).
      for W in 1 .. Img.Head.Waits loop
         declare
            Position : Natural := Used + 1;
         begin
            while Position > 1 and then
              Img.Waits (Order (Position - 1)).Duration < Img.Waits (W).Duration
            loop
               Order (Position) := Order (Position - 1);
               Position := Position - 1;
            end loop;
            Order (Position) := W;
            Used := Used + 1;
         end;
      end loop;
      Put ("BT-WAITS: blocking calls of at least " & Ms (Us_Of (Img.Head.Minimum_Wait)) &
           " ms, longest first (" & Decimal (Unsigned_64 (Img.Head.Waits)) & " recorded, " &
           Decimal (Unsigned_64 (Img.Head.Waits_Dropped)) & " dropped)" & LF);
      for Rank in 1 .. Natural'Min (Used, Ranked_Lines) loop
         declare
            W : constant Wait_Number := Order (Rank);
         begin
            Put ("BT-WAIT " & Pad (Decimal (Unsigned_64 (Rank)), 3) & " " &
                 Pad (Ms (Us_Of (Img.Waits (W).Duration)), 11) & " ms at " &
                 Pad (Ms (Us_Of (Img.Waits (W).Started)), 11) & "  " & Wait_Text (W) & LF);
         end;
      end loop;
   end Print_Waits;

   procedure Print_Processes is
   begin
      Put ("BT-PROCS: name, spawn->first instruction, first instruction->ready, " &
           "blocked in calls (count), asleep, other startup waits (ms)" & LF);
      for N in 1 .. Img.Head.Processes loop
         declare
            E : Process_Entry renames Img.Processes (N);
            Ready_T : constant Ticks := Ready_Of (E);
         begin
            Put ("BT-PROC " & Name_Of (N) & " " &
                 Pad (Ms (Us_Of (if E.First_Run /= 0 then E.First_Run - E.Spawn_Started else 0)), 10) & " " &
                 Pad ((if Ready_T /= 0 and then E.First_Run /= 0 and then Ready_T >= E.First_Run
                       then Ms (Us_Of (Ready_T - E.First_Run)) else "never"), 10) & " " &
                 Pad (Ms (Us_Of (E.Call_Ticks)), 10) & " (" & Decimal (Unsigned_64 (E.Calls)) & ") " &
                 Pad (Ms (Us_Of (E.Sleep_Ticks)), 10) & " " &
                 Pad (Ms (Us_Of (E.Wait_Ticks)), 10) & LF);
         end;
      end loop;
   end Print_Processes;

   procedure Print_Path (Goal : Milestone) is
      Path : Path_List;
      Count : Path_Count;
      Found : Boolean;
      Previous : Ticks := 0;
   begin
      Critical_Path (Img, Goal, Path, Count, Found);
      if not Found then
         Put ("BT-PATH " & Milestone_Name (Goal) & ": not reached" & LF);
         return;
      end if;
      Put ("BT-PATH " & Milestone_Name (Goal) & ": power-on -> goal along spawns and long waits" & LF);
      for I in 1 .. Count loop
         declare
            S : Step renames Path (I);
            What : constant String :=
              (case S.Kind is
                 when Power_On => "power-on (TSC reset)",
                 when Stage_Step => "kernel " & Stage_Name (S.Stage),
                 when Spawn_Step => "spawn " & Name_Of (S.Process),
                 when Run_Step => "first instruction " & Name_Of (S.Process),
                 when Wait_Step =>
                   (if S.Wait in 1 .. Img.Head.Waits
                    then Wait_Text (S.Wait) & " for " & Ms (Us_Of (Img.Waits (S.Wait).Duration)) & " ms"
                    else "?"),
                 when Goal_Step => "GOAL " & Milestone_Name (Goal) & " in " & Name_Of (S.Process));
         begin
            Put ("BT-PATH " & Pad (Ms (Us_Of (S.At_Ticks)), 11) & " " &
                 Pad ("+" & Ms (Us_Of (if S.At_Ticks >= Previous then S.At_Ticks - Previous else 0)), 11) &
                 "  " & What & LF);
            Previous := S.At_Ticks;
         end;
      end loop;
   end Print_Path;

   procedure Print_Budget is
      Frame : constant Ticks := Img.Milestones (Slot_Of (First_Frame_Presented)).At_Ticks;
      Input : constant Ticks := Img.Milestones (Slot_Of (First_Input_Accepted)).At_Ticks;
      Entry_T : constant Ticks := Img.Stages (Slot_Of (Kernel_Entry));
   begin
      if Frame /= Not_Recorded then
         Put ("BT-BUDGET first-frame=" & Ms (Us_Of (Frame)) & " ms from power-on, " &
              Ms (Us_Of (Frame - Entry_T)) & " ms from kernel entry; budget " &
              Decimal (Budget_Ms) & " ms: " &
              (if Us_Of (Frame) <= Budget_Ms * 1_000 then "within" else "OVER") &
              " (reported, not enforced)" & LF);
      end if;
      if Input /= Not_Recorded then
         Put ("BT-BUDGET first-input=" & Ms (Us_Of (Input)) & " ms from power-on" & LF);
      end if;
      Put ("BT-BUDGET firmware+loader=" & Ms (Us_Of (Entry_T)) & " ms (TSC at kernel entry)" & LF);
   end Print_Budget;

   Waited : Natural := 0;
   Result : Read_Result;
   Ignore : Unsigned_64;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   Put ("BOOT-TIMELINE: waiting for the first desktop frame" & LF);
   loop
      Result := Read_Image;
      exit when Result = Not_Granted or else Goal_Reached;
      exit when Waited >= Goal_Wait_Ms;
      Ignore := CuBit.Waits.Sleep_For (Poll_Ms);
      Waited := Waited + Poll_Ms;
   end loop;
   if Result = Not_Granted then
      Put ("BOOT-TIMELINE: FAIL boot-timeline publication not granted" & LF);
      return;
   end if;
   Ignore := CuBit.Waits.Sleep_For (Settle_Ms);
   Result := Read_Image;
   if not Valid_Image then
      Put ("BOOT-TIMELINE: FAIL timeline malformed (version" & Img.Head.Version'Image & ")" & LF);
      return;
   end if;
   if Img.Head.Frequency in Counter_Frequency then
      Frequency := Img.Head.Frequency;
   end if;
   Put ("BT-HEADER tsc-hz=" & Decimal (Img.Head.Frequency) &
        " processes=" & Decimal (Unsigned_64 (Img.Head.Processes)) &
        " waits=" & Decimal (Unsigned_64 (Img.Head.Waits)) &
        " dropped-processes=" & Decimal (Unsigned_64 (Img.Head.Processes_Dropped)) &
        " dropped-waits=" & Decimal (Unsigned_64 (Img.Head.Waits_Dropped)) &
        " recording=" & (if Img.Head.Recording = Open then "open" else "closed") & LF);
   Print_Timeline;
   Print_Waits;
   Print_Processes;
   Print_Path (First_Frame_Presented);
   Print_Budget;
   declare
      Gaps : Gap_Set;
      Complete : Boolean;
   begin
      Check (Img, Gaps, Complete);
      if Complete then
         Put ("BOOT-TIMELINE: COMPLETE" & LF);
      else
         Put ("BOOT-TIMELINE: INCOMPLETE");
         for G in Gap loop
            if Gaps (G) then
               Put (" " & (case G is
                             when No_Frequency      => "no-frequency",
                             when Stage_Missing     => "stage-missing",
                             when No_Processes      => "no-processes",
                             when Process_Missing   => "process-missing",
                             when Process_Unstarted => "process-unstarted",
                             when Process_Not_Ready => "process-not-ready",
                             when Milestone_Missing => "milestone-missing",
                             when Dropped_Records   => "dropped-records"));
            end if;
         end loop;
         Put (LF & "");
      end if;
   end;
end Main;
