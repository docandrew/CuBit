with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metric_Raw_Observer;
with CuBit.Metric_Protocol;
with Compositor_Trace_Stream;
with Compositor_Render_Trace;
with CCL_Manifest_Bindings;
with CuBit.Filesystems;
with CuBit.Memory_Grants;
with CuBit.Monotonic;
with Compositor_Trace_Framing;
with Archive_Test_Control;
procedure Main is
   package P renames CuBit.Metric_Protocol;
   package O renames CuBit.Metric_Raw_Observer;
   package S renames Compositor_Trace_Stream;
   package W renames S.W;
   use type P.Status, W.Event_Kind, Compositor_Render_Trace.Phase;
   Raw : O.Observer (CCL_Manifest_Bindings.Slot_metrics_observer);
   Collector : S.State;
   Page : P.Raw_Page;
   N : P.Raw_Row_Count;
   Cursor : Unsigned_64 := 1;
   Resume, Gap, Lost, Ignore : Unsigned_64;
   Result : P.Status;
   Started, Done : Boolean := False;
   Counts : array (W.Event_Kind) of Natural := [others => 0];
   Draws, Submits : Natural := 0;
   Publisher, Last_ID, Drops, Batch_Gaps, History_Loss : Unsigned_64 := 0;
   Capture : S.Capture;
   package F renames Compositor_Trace_Framing;
   package FS renames CuBit.Filesystems;
   package MG renames CuBit.Memory_Grants;
   use type FS.File_Handle, FS.Open_Options, F.Phase;
   Archive : F.State;
   type Output_Page is array (Natural range 0 .. 511) of Unsigned_64;
   Output : aliased Output_Page := [others => 0] with Alignment => 4096;
   Loan : MG.Grant_Reference;
   Handle : FS.File_Handle := FS.INVALID_FILE_HANDLE;
   Slots : Natural range 0 .. 16 := 0;
   Offset : Unsigned_64 := 0;
   Archive_Open, Budget_Full : Boolean := False;

   procedure Fail (Why : String) is
   begin
      debugPrint ("TEST: FAIL desktop-trace " & Why & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, 1);
      loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
   end Fail;
   function Clock return Unsigned_64 is
      V : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      if not V.Available then Fail ("archive clock unavailable"); return 0; end if;
      if V.Microseconds = Unsigned_64'Last then Fail ("archive clock invalid"); end if;
      return V.Microseconds;
   end Clock;
   procedure Write_Page is
      M : Message;
      Bytes : constant Unsigned_64 := Unsigned_64 (Slots) * 256;
   begin
      if Slots = 0 then return; end if;
      M := FS.Write_At_Request (Handle, Loan, Bytes, Offset);
      M.tag := capCall (CCL_Manifest_Bindings.Slot_filesystem, M, Wait_Forever);
      if M.tag.label /= FS.REPLY_OK or else M.words (0) /= Bytes then
         debugPrint ("ARCHIVE: write status=" & M.tag.label'Image & " bytes=" & M.words (0)'Image & ASCII.LF);
         Fail ("archive short or failed write");
      end if;
      Offset := Offset + Bytes;
      Slots := 0;
      if Archive_Test_Control.Exit_After_First_Write then
         debugPrint ("TEST: INJECT desktop-trace archive writer exit pid=" &
           syscall (SYSCALL_GETPID)'Image & " bytes=" & Offset'Image & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 17);
         loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
      end if;
   end Write_Page;
   procedure Append (Data : F.A.Chunk) is
   begin
      if Slots = 16 then Write_Page; end if;
      for I in Data'Range loop Output (Slots * 32 + I) := Data (I); end loop;
      Slots := Slots + 1;
   end Append;
   procedure Open_Archive (Endpoint : Unsigned_64) is
      Path : constant String := "@nvme:0/work/desktop-trace.cubittrace";
      Text : String (1 .. 4096) with Import, Address => Output'Address;
      M : Message;
      OK : Boolean;
      Now : constant Unsigned_64 := Clock;
      H : constant F.A.Chunk := F.Header ((Now + 1, Endpoint, Now, 256));
   begin
      MG.Create_Via_Capability (CCL_Manifest_Bindings.Slot_filesystem,
        Output'Address, 1, False, Loan, OK);
      if not OK then Fail ("archive grant"); end if;
      Text (1 .. Path'Length) := Path;
      M := FS.Open_Request (Loan, Path'Length,
        FS.OPEN_READ_WRITE or FS.OPEN_CREATE or FS.OPEN_EXCLUSIVE);
      M.tag := capCall (CCL_Manifest_Bindings.Slot_filesystem, M, Wait_Forever);
      Handle := FS.File_Handle (M.words (0));
      if M.tag.label /= FS.REPLY_OK or else Handle = FS.INVALID_FILE_HANDLE then
         debugPrint ("ARCHIVE: open status=" & M.tag.label'Image & ASCII.LF);
         Fail ("archive exclusive open");
      end if;
      F.Start (Archive, H);
      Append (H); Archive_Open := True;
   end Open_Archive;
   procedure Save_Event (Value : S.Capture) is
      Accepted : Boolean;
      Data : F.A.Chunk;
   begin
      if not F.Can_Append (Archive, Value) then
         if F.Events (Archive) = F.Info (Archive).Budget then Budget_Full := True;
         else Fail ("archive event identity"); end if;
         return;
      end if;
      Data := F.Event_Chunk (Archive, Value);
      F.Feed (Archive, Data, Accepted);
      if not Accepted then Fail ("archive event policy"); end if;
      Append (Data);
   end Save_Event;
   procedure Finish_Archive is
      M : Message;
      Accepted, Revoked : Boolean;
      Data : F.A.Chunk;
   begin
      if not Archive_Open then Fail ("archive never opened"); end if;
      Data := F.Footer (Archive, Clock,
        (if Budget_Full then F.Budget_Reached else F.Requested_Stop), S.Counts (Collector));
      F.Feed (Archive, Data, Accepted);
      if F.Status (Archive) /= F.Footer_Seen then Fail ("archive footer"); end if;
      Append (Data); Write_Page;
      M := FS.Flush_Request (Handle);
      M.tag := capCall (CCL_Manifest_Bindings.Slot_filesystem, M, Wait_Forever);
      if M.tag.label /= FS.REPLY_OK then
         debugPrint ("ARCHIVE: flush status=" & M.tag.label'Image & ASCII.LF);
         Fail ("archive flush not confirmed");
      end if;
      M := FS.Close_Request (Handle);
      M.tag := capCall (CCL_Manifest_Bindings.Slot_filesystem, M, Wait_Forever);
      if M.tag.label /= FS.REPLY_OK then Fail ("archive close"); end if;
      MG.Revoke (Loan, Revoked);
      for Retry in 1 .. 100 loop
         exit when MG.Retirement_Confirmed (Loan);
         Ignore := syscall (SYSCALL_SLEEP, 10);
      end loop;
      if not MG.Retirement_Confirmed (Loan) then Fail ("archive grant retirement"); end if;
      F.End_Of_File (Archive, 0);
      if F.Status (Archive) /= F.Complete then Fail ("archive completion"); end if;
      debugPrint ("TEST: PASS desktop-trace-file bytes=" & Offset'Image &
        " events=" & F.Events (Archive)'Image & " flush=confirmed retirement=confirmed" & ASCII.LF);
   end Finish_Archive;
begin
   -- Bounded collector workload. It may lag and lose history; identity and
   -- completeness checks never turn an incomplete fragment group into an event.
   for Tick in 1 .. 1000 loop
      for Drain in 1 .. 8 loop
         O.Query (Raw, Cursor, Page, N, Resume, Gap, Lost, Result);
         if Result /= P.OK then
            S.Discard_Partial (Collector);
            Fail ("raw query");
         end if;
         if not Started then
            if O.Incarnation (Raw) = 0 then Fail ("missing endpoint"); end if;
            S.Start (Collector, O.Incarnation (Raw), Cursor);
            Open_Archive (O.Incarnation (Raw));
            Started := True;
         end if;
         if Lost < History_Loss then Fail ("history loss decreased"); end if;
         History_Loss := Lost;
         for I in 0 .. N - 1 loop
            S.Feed (Collector, O.Incarnation (Raw), Page (I), Capture);
            if Capture.Success and then
              Capture.Pid = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP)
            then
               if Publisher = 0 then Publisher := Capture.Publisher; end if;
               if Capture.Publisher /= Publisher or else
                 Capture.Value.Event_ID <= Last_ID or else
                 Capture.Producer_Dropped < Drops or else Capture.Batch_Gaps < Batch_Gaps
               then Fail ("identity or monotonic counters"); end if;
               Save_Event (Capture);
               Last_ID := Capture.Value.Event_ID;
               Drops := Capture.Producer_Dropped;
               Batch_Gaps := Capture.Batch_Gaps;
               Counts (Capture.Value.Kind) := Counts (Capture.Value.Kind) + 1;
               if Capture.Value.Kind = W.Render_Event then
                  if Capture.Value.Render.Kind = Compositor_Render_Trace.Draw
                  then Draws := Draws + 1; else Submits := Submits + 1; end if;
               end if;
            end if;
         end loop;
         Cursor := Resume;
         exit when N = 0;
      end loop;
      Ignore := syscall (SYSCALL_SLEEP, 20);
   end loop;
   S.Discard_Partial (Collector);
   for Retry in 1 .. 100 loop
      O.Disconnect (Raw, Done);
      exit when Done;
      Ignore := syscall (SYSCALL_SLEEP, 10);
   end loop;
   if not Done then Fail ("grant retirement"); end if;
   if Counts (W.Input_Event) < 3 or else Counts (W.Source_Event) = 0 or else
     Counts (W.Frame_Event) < 3 or else Draws = 0 or else Submits < 3
   then
      debugPrint ("TRACE: input=" & Counts (W.Input_Event)'Image &
        " source=" & Counts (W.Source_Event)'Image & " frame=" & Counts (W.Frame_Event)'Image &
        " draws=" & Draws'Image & " submits=" & Submits'Image & ASCII.LF);
      Fail ("missing real Desktop event types");
   end if;
   Finish_Archive;
   debugPrint ("TEST: PASS desktop-trace input=" & Counts (W.Input_Event)'Image &
     " source=" & Counts (W.Source_Event)'Image & " frame=" & Counts (W.Frame_Event)'Image &
     " draws=" & Draws'Image & " submits=" & Submits'Image &
     " producer_dropped=" & Drops'Image & " batch_gaps=" & Batch_Gaps'Image &
     " history_loss=" & History_Loss'Image &
     " skipped_rows=" & S.Counts (Collector).Skipped_Rows'Image &
     " rejected_rows=" & S.Counts (Collector).Rejected_Rows'Image &
     " retirement=confirmed" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
