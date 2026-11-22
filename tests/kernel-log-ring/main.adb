------------------------------------------------------------------------------
--  Hosted tests for the kernel log ring (LOG-004): records and their text,
--  line levels, the reader's exact account of what it took and what the
--  kernel overwrote (overflow before the reader starts, a record torn while
--  copied, the per-pass budget), and the per-process output tail.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Kernel_Log_Publication; use Kernel_Log_Publication;
with Fault_Reports; use Fault_Reports;
with Simulated_Ring;

procedure Main is
   package Ring renames Simulated_Ring;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("kernel-log-ring: " & Name & " PASS");
      else
         Failures := Failures + 1;
         Put_Line ("kernel-log-ring: " & Name & " FAIL");
      end if;
   end Check;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   procedure Write_Lines (From, To : Unsigned_64) is
   begin
      for N in From .. To loop
         Ring.Write ("line " & Decimal (N), Time_Ms => N);
      end loop;
   end Write_Lines;

   function Delivered_In_Order (First, Last : Unsigned_64) return Boolean is
     (Ring.Delivered_Count = Natural (Last - First + 1)
      and then (for all I in 1 .. Ring.Delivered_Count =>
                  Ring.Delivered (I) = First + Unsigned_64 (I - 1)));

   Accounted : Unsigned_64;
   More : Boolean;
   R : Log_Record;
   Long : constant String (1 .. 300) := [others => 'x'];
begin
   --  Records ------------------------------------------------------------
   R := Make_Line (5, "a" & ASCII.HT & "b" & ASCII.ESC & "c", False);
   Check (R.Sequence = Writing and then Text_Of (R) = "a b?c" and then not Truncated (R)
          and then R.Source = Kernel_Source and then Origin_Of (R) = Kernel_Line,
          "controls cleaned, record numbered last");
   R := Make_Line (5, Long, False);
   Check (Used_Length (R) = Text_Bytes and then Truncated (R), "long line cut and flagged");
   R := Make_Line (5, "STUCK-CALL: a -> b 5s", False);
   Check (Level_Of (R) = Warning, "a line's level from its prefix");
   R := Make_Stop (5, 16#1_0000_0010#, "fault-check.app" & ASCII.NUL,
                   Stopped_Report (16#2_0000_0003#), Everyone);
   Check (Name_Of (R) = "fault-check.app" and then Level_Of (R) = Error
          and then Origin_Of (R) = Process_Stopped and then Used_Length (R) = 0
          and then Kind_Of (R.Fault) = Stopped_By_Process
          and then R.Fault.Stopper = 16#2_0000_0003#, "stop record fields");
   R.Length := Unsigned_16'Last;
   Check (Used_Length (R) = Text_Bytes, "a bad length is clamped by the reader");
   Check (Line_Level ("STUCK-CALL: a -> b 5s") = Warning
          and then Line_Level ("process killed: x") = Error
          and then Line_Level ("FATAL: y") = Critical
          and then Line_Level ("CPU started: 1") = Information
          and then Line_Level ("") = Information, "line levels");

   --  Numbering -------------------------------------------------------------
   Check (Slot_Of (1, 64) = 0 and then Slot_Of (64, 64) = 63 and then Slot_Of (65, 64) = 0,
          "slots wrap");
   Check (First_Available (0, 500, 64) = 437 and then First_Available (0, 64, 64) = 1
          and then First_Available (436, 500, 64) = 437 and then First_Available (499, 500, 64) = 500,
          "first available");

   --  No overflow: everything, in order --------------------------------------
   Ring.Reset (Maximum_Records);
   Write_Lines (1, 10);
   Ring.Drain (256, Accounted, More);
   Check (Delivered_In_Order (1, 10) and then Ring.Run_Count = 0 and then Accounted = 10
          and then not More and then Ring.Delivered_Text_Ok and then Ring.Seen = 10,
          "ten records, none dropped");
   Ring.Drain (256, Accounted, More);
   Check (Accounted = 0 and then Ring.Delivered_Count = 10, "nothing new, nothing taken");

   --  Overflow before the reader starts (the shrunken test kernel) ----------
   Ring.Reset (64);
   Write_Lines (1, 500);
   Ring.Drain (256, Accounted, More);
   Check (Ring.Run_Count = 1 and then Ring.Runs (1).First = 1 and then Ring.Runs (1).Last = 436
          and then Delivered_In_Order (437, 500) and then Accounted = 500 and then not More
          and then Ring.Delivered_Text_Ok,
          "overflow: 436 dropped (sequence 1..436), 437..500 taken");

   --  The reader falls behind again by exactly one ring ----------------------
   Write_Lines (501, 565);
   Ring.Drain (256, Accounted, More);
   Check (Ring.Run_Count = 2 and then Ring.Runs (2).First = 501 and then Ring.Runs (2).Last = 501
          and then Ring.Delivered_Count = 64 + 64 and then Accounted = 65,
          "lapped by one: 1 dropped (sequence 501..501)");

   --  A record overwritten while the reader copies it ------------------------
   Ring.Reset (64);
   Write_Lines (1, 20);
   Ring.Begin_Overwrite (7);
   Ring.Begin_Overwrite (8);
   Ring.Drain (256, Accounted, More);
   Check (Ring.Run_Count = 1 and then Ring.Runs (1).First = 7 and then Ring.Runs (1).Last = 8
          and then Ring.Delivered_Count = 18 and then Accounted = 20,
          "torn records 7..8 dropped as one run, the rest taken");
   Ring.Reset (64);
   Write_Lines (1, 20);
   Ring.Begin_Overwrite (20);
   Ring.Drain (256, Accounted, More);
   Check (Ring.Run_Count = 1 and then Ring.Runs (1).First = 20 and then Ring.Runs (1).Last = 20
          and then Accounted = 20, "a run at the end is reported");

   --  The budget: a late reader catches up over several passes ---------------
   Ring.Reset (Maximum_Records);
   Write_Lines (1, 600);
   Ring.Drain (256, Accounted, More);
   Check (Accounted = 256 and then More and then Ring.Seen = 256, "budget: first pass 256");
   Ring.Drain (256, Accounted, More);
   Ring.Drain (256, Accounted, More);
   Check (Accounted = 88 and then not More and then Delivered_In_Order (1, 600)
          and then Ring.Run_Count = 0, "budget: 600 records over three passes, in order");

   --  Fault reports -----------------------------------------------------------
   declare
      --  A process laid out as CuBit's loader and Process place one.
      L : constant Layout :=
        (Stack_First => 16#7F_0000_0000#, Stack_Limit => 16#7F_0100_0000#,
         Heap_First  => 16#60_0000#,      Heap_Limit  => 16#70_0000#,
         Image_First => 16#40_0000#,      Image_Limit => 16#50_0000#);
      User_Write_Not_Present : constant := 2#110#;
      User_Write_Protection  : constant := 2#111#;
      User_Fetch_Protection  : constant := 2#10101#;
      Kernel_Read_Reserved   : constant := 2#1001#;
      F : Report;
   begin
      Check (Decode_Page_Fault (User_Write_Not_Present) = (Write, Not_Present, User_Mode)
             and then Decode_Page_Fault (User_Write_Protection) = (Write, Protection, User_Mode)
             and then Decode_Page_Fault (User_Fetch_Protection) = (Execute, Protection, User_Mode)
             and then Decode_Page_Fault (Kernel_Read_Reserved) = (Read, Reserved_Entry, Kernel_Mode),
             "page-fault error codes decoded");
      Check (Kind_Of_Vector (13) = General_Protection and then Kind_Of_Vector (6) = Invalid_Opcode
             and then Kind_Of_Vector (0) = Divide_Error and then Kind_Of_Vector (17) = Alignment_Check
             and then Kind_Of_Vector (19) = SIMD_Floating_Point and then Kind_Of_Vector (5) = Bound_Range
             and then Kind_Of_Vector (12) = Stack_Segment and then Kind_Of_Vector (16) = X87_Floating_Point
             and then Kind_Of_Vector (99) = Other_Exception, "vectors named");
      F := Exception_Report (14, User_Write_Not_Present, 16#10#, 16#40_1A3F#, 16#7F_00FF_F000#, L);
      Check (Kind_Of (F) = Page_Fault and then Operation_Of (F) = Write and then Where_Of (F) = Null_Page
             and then Stack_Of (F) = In_Stack_Reservation
             and then Anchor_Value (F.RIP_From) = Image and then F.RIP = 16#1A3F#
             and then Anchor_Value (F.Address_From) = Zero and then F.Address = 16#10#
             and then Anchor_Value (F.RSP_From) = Stack_Top and then F.RSP = 16#1000#,
             "null write: null page, image-relative rip, stack-relative rsp");
      --  The two audiences: the summary has no address, offset or register;
      --  a reader without diagnostics authority sees only it, one with it
      --  sees both.
      declare
         Summary : constant Log_Record :=
           Make_Stop (5, 16#1_0000_0010#, "fault-check.app" & ASCII.NUL, F, Everyone);
         Detail  : constant Log_Record :=
           Make_Stop (5, 16#1_0000_0010#, "fault-check.app" & ASCII.NUL, F, Diagnostics);
         function Seen (Reader : Audience) return Natural is
           ((if Visible (Summary, Reader) then 1 else 0) + (if Visible (Detail, Reader) then 1 else 0));
      begin
         Check (Summary.Fault.Address = 0 and then Summary.Fault.RIP = 0 and then Summary.Fault.RSP = 0
                and then Summary.Fault.Where = 0 and then Summary.Fault.Error_Code = 0
                and then Kind_Of (Summary.Fault) = Page_Fault and then Operation_Of (Summary.Fault) = Write,
                "the summary keeps the kind and access, no addresses");
         Check (Detail.Fault = F and then Audience_Of (Detail) = Diagnostics, "the detail keeps the report");
         Check (Seen (Everyone) = 1 and then Visible (Summary, Everyone) and then Seen (Diagnostics) = 2,
                "a reader without diagnostics authority gets the summary only; with it, both");
      end;
      F := Exception_Report (14, User_Write_Not_Present, 16#7E_FFFF_FFF8#, 16#40_0010#,
                             16#7E_FFFF_FFF0#, L);
      Check (Where_Of (F) = Stack_Overflow and then Stack_Of (F) = In_Stack_Guard
             and then Anchor_Value (F.Address_From) = Stack_Bottom and then F.Address = 8,
             "write below the stack: stack overflow, rsp in the guard, offset below the stack");
      F := Exception_Report (14, User_Write_Protection, 16#40_0100#, 16#40_0010#, 16#7F_00FF_F000#, L);
      Check (Where_Of (F) = Read_Only, "write to the image: read-only");
      F := Exception_Report (14, User_Fetch_Protection, 16#60_0000#, 16#60_0000#, 16#7F_00FF_F000#, L);
      Check (Where_Of (F) = No_Execute and then Operation_Of (F) = Execute, "fetch from the heap: no-execute");
      F := Exception_Report (14, User_Write_Not_Present, 16#2000_0000#, 16#40_0010#, 16#7F_00FF_F000#, L);
      Check (Where_Of (F) = Unmapped, "write far from everything: unmapped");
      F := Exception_Report (14, 2#100#, 16#FFFF_8000_0000_0000#, 16#40_0010#, 16#7F_00FF_F000#, L);
      Check (Where_Of (F) = Kernel_Space, "read of kernel space");
      F := Exception_Report (13, 0, 0, 16#90_0000#, 16#7F_00FF_F000#, L);
      Check (Kind_Of (F) = General_Protection and then Where_Of (F) = Not_Applicable
             and then Anchor_Value (F.RIP_From) = Unanchored and then F.RIP = 16#90_0000#,
             "#GP outside the image: rip unanchored");
      F := Out_Of_Memory_Report (16#60_1000#, 16#40_0010#, 16#7F_00FF_F000#, L);
      Check (Kind_Of (F) = Out_Of_Memory and then Where_Of (F) = In_Heap, "out of memory in the heap");
   end;

   if Failures = 0 then
      Put_Line ("kernel-log-ring: PASS");
   else
      Put_Line ("kernel-log-ring: FAIL (" & Natural'Image (Failures) & " checks)");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
