--  Hosted tests for the libc's Ada (docs/c-removal.md): the child table
--  posix_spawn and waitpid keep.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C; use type Interfaces.C.int;
with CuBit.Child_Exits; use CuBit.Child_Exits;
with CuBit.Child_Table; use CuBit.Child_Table;
with CuBit.Process_IDs; use CuBit.Process_IDs;
with CuBit.Libc_ABI;
with CuBit.Libc_Time;
with CuBit.Libc_Select;
with CuBit.Libc_Reports;
with CuBit.Launch_Arguments;
with CuBit.Libc_Start_Layout;
with CuBit.Libc_Rings;
with CuBit.Libc_Directory_Entries;
with CuBit.Directory_Pages;
with CuBit.Libc_Descriptor_Rules;
with CuBit.Libc_File_Cache;
with CuBit.Libc_Park_Table;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   T : Table;
   Found : Interfaces.C.int;
   Status : Wait_Status;
   --  A process identity (KERN-003): slot and generation.
   function Id (Slot, Generation : Unsigned_64) return Process_ID is
     (From_Word (Generation * 2 ** 24 + Slot));
   --  Its pid_t.
   function PID (Slot, Generation : Unsigned_64) return Interfaces.C.int is
     (POSIX_Of (Id (Slot, Generation)));
begin
   Check (not Has_Child (T, -1) and then not Has_Child (T, PID (5, 1)), "empty table");
   Take (T, -1, Found, Status);
   Check (Found = 0, "nothing ended yet");

   Started (T, Id (5, 100));
   Started (T, Id (6, 101));
   Started (T, Id (7, 102));
   Check (Has_Child (T, -1) and then Has_Child (T, 0) and then Has_Child (T, PID (6, 101))
          and then not Has_Child (T, PID (8, 1)), "live children");

   --  An earlier life of the same slot (a retired identity) changes nothing.
   Exited (T, (Process => Id (6, 99), Kind => CuBit.Child_Exits.Exited, Code => 3));
   Check (T.Ended_Count = 0 and then Has_Child (T, PID (6, 101)), "stale identity ignored");
   --  Nor does a process we never started.
   Exited (T, (Process => Id (9, 101), Kind => CuBit.Child_Exits.Exited, Code => 3));
   Check (T.Ended_Count = 0, "unknown process ignored");

   Exited (T, (Process => Id (6, 101), Kind => CuBit.Child_Exits.Exited, Code => 255));
   Exited (T, (Process => Id (5, 100), Kind => Stopped, Code => 0));
   Check (T.Ended_Count = 2 and then T.Live_Count = 1
          and then not Has_Child (T, PID (6, 101)) and then Has_Child (T, PID (7, 102)),
          "two ended, one live");

   Take (T, PID (7, 102), Found, Status);
   Check (Found = 0, "a live child is not taken");
   Take (T, -1, Found, Status);
   Check (Found = PID (6, 101) and then Status = 255 * 256, "oldest first: exit code 255");
   Take (T, PID (5, 100), Found, Status);
   Check (Found = PID (5, 100) and then Status = 9, "stopped: SIGKILL status");
   Take (T, -1, Found, Status);
   Check (Found = 0, "all ended children taken");

   --  The same slot again, a later generation: a different pid_t.
   Check (PID (6, 200) /= PID (6, 101), "a reused slot is a new pid_t");
   Started (T, Id (6, 200));
   Exited (T, (Process => Id (6, 200), Kind => CuBit.Child_Exits.Exited, Code => 1));
   Take (T, PID (6, 200), Found, Status);
   Check (Found = PID (6, 200) and then Status = 256, "slot reuse with a new generation");

   --  pid_t is positive whatever the generation (its top bit is never set).
   Check (PID (16#FF_FFFF#, 16#FF_FFFF_FFFF#) > 0, "pid_t stays positive");

   --  Full: Capacity children at once; one more is dropped.
   T := (others => <>);
   for K in 1 .. Unsigned_64 (Capacity) loop
      Started (T, Id (K, 1));
   end loop;
   Check (T.Live_Count = Capacity, "capacity children fit");
   Started (T, Id (1, 2));
   Check (T.Live_Count = Capacity, "past capacity: dropped");

   ----------------------------------------------------------------- time
   declare
      use CuBit.Libc_Time;
      use CuBit.Libc_ABI;
      Big : constant Integer_64 := Integer_64'Last;
   begin
      Check (Milliseconds (Timespec'(1, 1)) = 1_001, "ms rounds up");
      Check (Milliseconds (Timespec'(2, 0)) = 2_000, "whole seconds");
      Check (Milliseconds (Timespec'(0, 999_999_999)) = 1_000, "just under a second");
      Check (Microseconds (Timespec'(0, 1_001)) = 2, "us rounds up");
      Check (Milliseconds (Timespec'(Big, 999_999_999)) = Forever, "huge saturates");
      Check (Microseconds (Timespec'(Big, 0)) = Forever, "us past range saturates");
      Check (Microseconds (Timespec'(Big / 1_000_000, 0)) /= Forever,
             "us near the top of Integer_64 still fits");
      Check (Milliseconds (Timeval'(1, 1)) = 1_001, "timeval rounds up");
      Check (not Valid (Timespec'(-1, 0)) and then not Valid (Timespec'(0, 1_000_000_000))
             and then not Valid (Timespec'(0, -1)), "invalid times");
      Check (After (Unsigned_64'Last - 5, 10) = Unsigned_64'Last, "deadline saturates");
      Check (Monotonic_Deadline (100, 5_000, 4_000) = 100, "past: now");
      Check (Monotonic_Deadline (100, 5_000, 6_001) = 102, "future rounds up");
      Check (Wall_Deadline (100, 1_000, 1_500) = 600, "wall deadline");
      Check (Wall_Deadline (100, 1_000, 900) = 100, "wall past: now");
      Check (From_Milliseconds (1_234) = Timespec'(1, 234_000_000), "from ms");
      Check (From_Nanoseconds (1_000_000_001) = Timespec'(1, 1), "from ns");
      Check (From_Nanoseconds (Unsigned_64'Last)
               = Timespec'(18_446_744_073, 709_551_615), "from ns, largest");
      --  Differential: random valid times against wide arithmetic.
      declare
         Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
         Agree : Natural := 0;
         Trials : constant := 200_000;
      begin
         for T in 1 .. Trials loop
            Seed := Seed xor Shift_Left (Seed, 13);
            Seed := Seed xor Shift_Right (Seed, 7);
            Seed := Seed xor Shift_Left (Seed, 17);
            declare
               S : constant Integer_64 :=
                 Integer_64 (Seed mod (if T mod 3 = 0 then 2 ** 62 else 2 ** 34));
               N : constant Integer_64 := Integer_64 (Shift_Right (Seed, 3) mod 1_000_000_000);
               Want : constant Long_Long_Long_Integer :=
                 Long_Long_Long_Integer (S) * 1_000
                 + (Long_Long_Long_Integer (N) + 999_999) / 1_000_000;
               Got : constant Unsigned_64 := Milliseconds (Timespec'(S, N));
            begin
               if (if Want > Long_Long_Long_Integer (Unsigned_64'Last)
                   then Got = Forever
                   else Long_Long_Long_Integer (Got) = Want)
               then
                  Agree := Agree + 1;
               end if;
            end;
         end loop;
         Check (Agree = Trials, "random times agree with wide arithmetic");
      end;
   end;

   --------------------------------------------------------------- select
   declare
      use CuBit.Libc_Select;
      use CuBit.Libc_ABI;
      R, W, X : Descriptor_Set := [others => False];
      Polls : Poll_Array;
      Used : Limit;
      Ready : Natural;
      Invalid : Boolean;
      R_Out, W_Out, X_Out : Descriptor_Set;
   begin
      R (3) := True;
      W (3) := True;
      W (7) := True;
      X (9) := True;
      R (12) := True;                     --  at or past the limit: ignored
      Gather (12, (True, True, True), R, W, X, Polls, Used);
      Check (Used = 3 and then Polls (0).Descriptor = 3
             and then Polls (0).Events = POLLIN + POLLOUT
             and then Polls (1).Descriptor = 7 and then Polls (2).Descriptor = 9
             and then Polls (2).Events = POLLPRI, "gather in order, below the limit");
      Polls (0).Returned := POLLHUP;      --  hang-up counts as readable
      Polls (1).Returned := POLLOUT;
      Polls (2).Returned := 0;
      Scatter (Polls, Used, (True, True, True), R_Out, W_Out, X_Out, Ready, Invalid);
      Check (not Invalid and then Ready = 2 and then R_Out (3) and then not W_Out (3)
             and then W_Out (7) and then not X_Out (9), "scatter ready bits");
      Polls (1).Returned := POLLNVAL;
      Scatter (Polls, Used, (True, True, True), R_Out, W_Out, X_Out, Ready, Invalid);
      Check (Invalid, "POLLNVAL: EBADF");
      Gather (12, (True, False, False), R, W, X, Polls, Used);
      Check (Used = 1 and then Polls (0).Events = POLLIN, "absent sets are not read");
      Check (Descriptor_Set'Size = FD_SETSIZE, "fd_set is 1024 bits");
   end;

   -------------------------------------------------------------- reports
   declare
      use CuBit.Libc_Reports;
      Text : CuBit.Libc_Reports.Line;
      Length : CuBit.Libc_Reports.Line_Length;
      Seen : Seen_Table;
      New_One : Boolean;
   begin
      Format ("unsupported", "clock", 11, Text, Length);
      Check (Text (1 .. Length) = "cubit-libc: unsupported clock 11" & Character'Val (10),
             "report line");
      Format ("unsupported", "x", Integer_64'First, Text, Length);
      Check (Text (1 .. Length) = "cubit-libc: unsupported x -9223372036854775808"
             & Character'Val (10), "most negative value prints");
      Format ([1 .. What_Bytes => 'p'], [1 .. 200 => 'w'], Integer_64'First,
              Text, Length);
      Check (Length = 12 + What_Bytes + 1 + What_Bytes + 1 + 1 + 19 + 1
             and then Length <= Line_Bytes, "longest line fits");
      First_Time (Seen, "clock", 5, New_One);
      Check (New_One, "first report");
      First_Time (Seen, "clock", 5, New_One);
      Check (not New_One, "repeat suppressed");
      First_Time (Seen, "clock", 6, New_One);
      Check (New_One, "another value reports");
   end;

   ---------------------------------------------------- start: string layout
   declare
      use CuBit.Launch_Arguments;
      B : Builder;
      Length : Present_Length;
      Accepted : Boolean;
      Firsts : CuBit.Libc_Start_Layout.Starts;
      Found : String_Count;
      Want : constant array (1 .. 4) of access constant String :=
        [new String'("prog"), new String'(""), new String'("A=1"),
         new String'("@nvme:0/src")];
   begin
      Start (B);
      Add_Argument (B, Want (1).all, Accepted);
      Add_Argument (B, Want (2).all, Accepted);
      Add_Environment (B, Want (3).all, Accepted);
      Add_Directory (B, Want (4).all, Accepted);
      Finish (B, Length, Accepted);
      CuBit.Libc_Start_Layout.Locate_Strings (B.Data (1 .. Length), Firsts, Found);
      Check (Accepted and then Found = 4, "layout finds every string");
      for K in 1 .. Found loop
         declare
            Last : Natural := Firsts (K) - 1;
         begin
            while B.Data (Last + 1) /= 0 loop
               Last := Last + 1;
            end loop;
            Check (Last - Firsts (K) + 1 = Want (K)'Length, "string" & K'Image & " length");
         end;
      end loop;
      --  An attached description is not strings.
      Attach_Description (B.Data, Length, [16#50#, 16#44#, 16#53#, 16#43#, 0, 0], Accepted);
      CuBit.Libc_Start_Layout.Locate_Strings (B.Data (1 .. Length), Firsts, Found);
      Check (Accepted and then Found = 4
             and then (for all K in 1 .. Found => Firsts (K) <= Strings_Last (B.Data (1 .. Length))),
             "a description after the strings adds none");
      Start (B);
      Finish (B, Length, Accepted);
      CuBit.Libc_Start_Layout.Locate_Strings (B.Data (1 .. Length), Firsts, Found);
      Check (Found = 0, "an empty block has no strings");
   end;

   --------------------------------------------------------- descriptor rules
   declare
      use CuBit.Libc_Descriptor_Rules;
      use CuBit.Libc_ABI;
      use type Interfaces.C.long;
      Result : Unsigned_64;
      Valid : Boolean;
      Options : Unsigned_64;
   begin
      Seek (SEEK_SET, 10, 5, 100, Result, Valid);
      Check (Valid and then Result = 10, "seek set");
      Seek (SEEK_CUR, -5, 5, 100, Result, Valid);
      Check (Valid and then Result = 0, "seek back to zero");
      Seek (SEEK_CUR, -6, 5, 100, Result, Valid);
      Check (not Valid, "seek before the start");
      Seek (SEEK_END, Integer_64'Last, 1, 100, Result, Valid);
      Check (not Valid, "seek past the representable end (C overflowed)");
      Seek (SEEK_SET, Integer_64'First, 0, 0, Result, Valid);
      Check (not Valid, "most negative offset");
      Seek (7, 0, 0, 0, Result, Valid);
      Check (not Valid, "unknown whence");
      Open_Options (O_RDWR + O_CREAT + O_EXCL + O_TRUNC, Options, Valid);
      Check (Valid and then Options = OPEN_READ_WRITE + OPEN_CREATE + OPEN_EXCLUSIVE
             + OPEN_TRUNCATE, "open options");
      Open_Options (O_WRONLY + O_EXCL, Options, Valid);
      Check (Valid and then Options = OPEN_WRITE_ONLY, "O_EXCL without O_CREAT is ignored");
      Open_Options (8#10000000#, Options, Valid);
      Check (not Valid, "O_PATH access mode refused");
      Check (Set_Status_Flags (O_RDWR, O_NONBLOCK) = O_RDWR + O_NONBLOCK
             and then Set_Status_Flags (O_RDWR + O_NONBLOCK, 0) = O_RDWR, "F_SETFL");
      Check (Blocks (0) = 0 and then Blocks (1) = 1 and then Blocks (512) = 1
             and then Blocks (Unsigned_64'Last) = Unsigned_64'Last / 512 + 1, "st_blocks");
   end;

   ------------------------------------------------------------- pipe ring
   declare
      use CuBit.Libc_Rings;
      R : Ring;
      Done, Got : Ring_Count;
      Source : Bytes (1 .. 70_000);
      Out_Buffer : Bytes (1 .. 70_000);
      Ok : Boolean := True;
   begin
      for I in Source'Range loop
         Source (I) := Unsigned_8 (I mod 251);
      end loop;
      Put (R, Source (1 .. 10), Done);
      Take (R, Out_Buffer (1 .. 4), Got);
      Check (Done = 10 and then Got = 4 and then Out_Buffer (1 .. 4) = Source (1 .. 4),
             "ring: in order");
      Put (R, Source (11 .. 70_000), Done);
      Check (Done = Ring_Bytes - 6, "ring: a short write fills it");
      Take (R, Out_Buffer, Got);
      Check (Got = Ring_Bytes, "ring: drains");
      for I in 1 .. Got loop
         Ok := Ok and then Out_Buffer (I) = Source (I + 4);
      end loop;
      Check (Ok, "ring: wrap-around keeps order");
   end;

   --------------------------------------------------------- directory entries
   declare
      package D renames CuBit.Libc_Directory_Entries;
      package DP renames CuBit.Directory_Pages;
      P : DP.Page;
      W : DP.Writer;
      Valid, Ended, Fits, OK : Boolean;
      Count : DP.Entry_Count;
      Page_Used : DP.Used_Bytes;
      Resume, Stamp : Unsigned_64;
      Buffer : D.Bytes (0 .. 63) := [others => 16#AA#];
      Used : Natural := 0;
      Next, Second : Natural;
      Name : constant String := "hello.txt";
      Bytes : DP.Name_Bytes := [others => 0];
   begin
      for K in Name'Range loop
         Bytes (K) := Character'Pos (Name (K));
      end loop;
      DP.Start (P, W);
      DP.Append (P, W, (Kind => DP.Kind_File, Object => 42, others => <>), Bytes, Name'Length);
      DP.Append (P, W, (Kind => DP.Kind_Directory, Object => 43, others => <>), Bytes, Name'Length);
      DP.Finish (P, W, True, 0, 0);
      DP.Check (P, Valid, Count, Page_Used, Ended, Resume, Stamp);
      Check (Valid and then Count = 2 and then Ended, "page header");
      D.Encode (P, DP.Header_Bytes, Page_Used, Buffer, Used, Fits, OK, Next);
      Check (Fits and then OK and then Used = 32 and then Buffer (0) = 42 and then Buffer (16) = 32
             and then Buffer (18) = D.DT_REG and then Buffer (19) = Character'Pos ('h')
             and then Buffer (19 + Name'Length) = 0, "dirent record");
      D.Encode (P, Next, Page_Used, Buffer, Used, Fits, OK, Second);
      Check (Fits and then OK and then Used = 64 and then Buffer (32 + 18) = D.DT_DIR,
             "second record fits exactly");
      D.Encode (P, Next, Page_Used, Buffer, Used, Fits, OK, Second);
      Check (OK and then not Fits and then Used = 64, "a full buffer refuses the next");
      D.Encode (P, Next + 8, Page_Used, Buffer, Used, Fits, OK, Second);
      Check (not OK and then Used = 64, "a bad offset is refused");
   end;

   --------------------------------------------------------------- page cache
   declare
      use CuBit.Libc_File_Cache;
      type Cache_Access is access Cache;
      C : constant Cache_Access := new Cache;
      F, G : File_Slot;
      Found : Boolean;
      Slot, Again : Page_Link;
      None_Used : constant File_Uses := [others => False];
   begin
      File_For (C.all, 16#1_0000_0005#, 1, None_Used, F, Found);
      Check (Found, "a file entry");
      New_Page (C.all, F, 3, Can_Grow => True, Slot => Slot);
      Find (C.all, F, 3, Again);
      Check (Slot /= No_Link and then Again = Slot, "a page found again");
      Find (C.all, F, 4, Again);
      Check (Again = No_Link, "another page is not there");
      File_For (C.all, 16#1_0000_0005#, 2, None_Used, G, Found);
      Find (C.all, F, 3, Again);
      Check (Found and then G = F and then Again = No_Link,
             "a new version makes the old pages stale");
      New_Page (C.all, F, 9, Can_Grow => False, Slot => Again);
      Check (Again = Slot, "a stale slot is reused before growing");
      for P in 1 .. 100 loop
         New_Page (C.all, F, Unsigned_64 (100 + P), Can_Grow => P <= 10, Slot => Again);
         Check (Again /= No_Link and then Again <= 11, "the clock reuses backed slots");
         exit when Failures > 0;
      end loop;
      Check (C.Pages_Backed = 11, "growth stops when the caller has no buffer");
   end;

   ------------------------------------------------------------- park table
   declare
      use CuBit.Libc_Park_Table;
      type Table_Access is access CuBit.Libc_Park_Table.Table;
      T : constant Table_Access := new CuBit.Libc_Park_Table.Table;
      Found : Link;
   begin
      Set_Name (T.all, 5, "@nvme:0/a");
      Set_Name (T.all, 9, "@nvme:0/b");
      Insert (T.all, 5);
      Insert (T.all, 9);
      Find (T.all, "@nvme:0/a", Found);
      Check (Found = 6, "park: find by name");
      Find (T.all, "@nvme:0/c", Found);
      Check (Found = No_Link, "park: an unparked name");
      Check (T.Oldest = 6 and then T.Newest = 10 and then T.Count = 2, "park: recency");
      Remove (T.all, 5);
      Find (T.all, "@nvme:0/a", Found);
      Check (Found = No_Link and then T.Oldest = 10 and then T.Count = 1, "park: removal");
   end;

   if Failures = 0 then
      Put_Line ("libc-ada:" & Checks'Image & " checks PASS");
   else
      Put_Line ("libc-ada:" & Failures'Image & " of" & Checks'Image & " checks FAIL");
   end if;
end Main;
