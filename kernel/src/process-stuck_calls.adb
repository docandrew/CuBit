-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Process.Stuck_Calls - implementation.
-------------------------------------------------------------------------------
with Boot_Diagnostics;
with Spinlocks;
with Stuck_Watch_Text;
with TextIO;
with Time;

package body Process.Stuck_Calls is

    --  The watch (reportStuckCalls): per thread, its CPU and dispatch
    --  counts at the previous report, when it was last dispatched, and how
    --  many reports in a row it was hot. Touched only by the timer of the
    --  CPU keeping time (Time.clockTick's keeper, one at a time).
    type Watch_Sample is record
        residency : Unsigned_64 := 0;
        dispatches : Unsigned_64 := 0;
        quietSince : Unsigned_64 := 0;
        hotReports : Natural := 0;
        percent : Unsigned_64 := 0;
        watched : Boolean := False;
        --  This report: the thread was hot for Watch_Hot_Reports reports,
        --  or is a main thread waiting undispatched for Stuck_Call_Ms.
        firedHot : Boolean := False;
        firedQuiet : Boolean := False;
    end record;
    watchSamples  : array (ThreadID) of Watch_Sample;
    lastReportMs  : Unsigned_64 := 0;
    Watch_Hot_Percent : constant := 90;
    Watch_Hot_Reports : constant := 2;
    Watched_Names : constant array (1 .. 4) of String (1 .. 9) :=
      ("desktop.s", "display.s", "intel-gpu", "virtio-gp");

    --  noteEvidence's rows, newest last, under evidenceLock.
    evidenceLock  : Spinlocks.Spinlock;
    evidenceText  : array (1 .. Evidence_Rows) of Boot_Diagnostics.Stuck_Line :=
      (others => (others => ' '));
    evidenceCount : Natural range 0 .. Evidence_Rows := 0;
    evidenceAtMs  : Unsigned_64 := 0;

    procedure noteEvidence (text : String) is
        row : Boot_Diagnostics.Stuck_Line := (others => ' ');
        used : constant Natural := Natural'Min (text'Length, row'Length);
    begin
        row (1 .. used) := text (text'First .. text'First + used - 1);
        Spinlocks.enterCriticalSection (evidenceLock);
        if evidenceCount = Evidence_Rows then
            for i in 1 .. Evidence_Rows - 1 loop
                evidenceText (i) := evidenceText (i + 1);
            end loop;
        else
            evidenceCount := evidenceCount + 1;
        end if;
        evidenceText (evidenceCount) := row;
        evidenceAtMs := Time.msTicks;
        Spinlocks.exitCriticalSection (evidenceLock);
    end noteEvidence;

    procedure reportStuckCalls (nowMs : Unsigned_64) is
        lines : Boot_Diagnostics.Stuck_Lines :=
          (others => (others => ' '));
        count : Natural := 0;

        procedure add (line : in out Boot_Diagnostics.Stuck_Line;
                       last : in out Natural; text : String) is
        begin
            for c of text loop
                exit when last = line'Last;
                last := last + 1;
                line (last) := c;
            end loop;
        end add;

        procedure addNumber (line : in out Boot_Diagnostics.Stuck_Line;
                             last : in out Natural; value : Unsigned_64) is
            numerals : String (1 .. 20);
            first   : Natural := numerals'Last + 1;
            rest    : Unsigned_64 := value;
        begin
            loop
                first := first - 1;
                numerals (first) := Character'Val (Character'Pos ('0') + Natural (rest mod 10));
                rest := rest / 10;
                exit when rest = 0;
            end loop;
            add (line, last, numerals (first .. numerals'Last));
        end addNumber;

        procedure addHex (line : in out Boot_Diagnostics.Stuck_Line;
                          last : in out Natural; value : Unsigned_64) is
            hexDigits : constant String := "0123456789ABCDEF";
            numerals  : String (1 .. 16);
        begin
            for i in numerals'Range loop
                numerals (i) := hexDigits
                  (Natural (Shift_Right (value, (16 - i) * 4) and 16#F#) + 1);
            end loop;
            add (line, last, numerals);
        end addHex;

        procedure addShortHex (line : in out Boot_Diagnostics.Stuck_Line;
                               last : in out Natural; value : Unsigned_32) is
            hexDigits : constant String := "0123456789ABCDEF";
            numerals  : String (1 .. 8);
        begin
            for i in numerals'Range loop
                numerals (i) := hexDigits
                  (Natural (Shift_Right (value, (8 - i) * 4) and 16#F#) + 1);
            end loop;
            add (line, last, numerals);
        end addShortHex;

        procedure addName (line : in out Boot_Diagnostics.Stuck_Line;
                           last : in out Natural; pid : ProcessID) is
            name : constant ProcessName := proctab (pid).name;
            stop : Natural := 0;
        begin
            for i in name'Range loop
                exit when name (i) = ASCII.NUL;
                if name (i) /= ' ' then stop := i; end if;
            end loop;
            add (line, last, name (name'First .. stop));
            add (line, last, "(");
            addNumber (line, last, Unsigned_64 (pid));
            add (line, last, ")");
        end addName;

        function stateClass (s : ProcessState) return Stuck_Watch_Text.Wait_Class is
          (case s is
             when INVALID              => Stuck_Watch_Text.Gone,
             when READY                => Stuck_Watch_Text.Ready,
             when RUNNING              => Stuck_Watch_Text.Running,
             when SLEEPING             => Stuck_Watch_Text.Sleeping,
             when WAITING              => Stuck_Watch_Text.Waiting,
             when WAITINGFOREVENT      => Stuck_Watch_Text.Waiting_For_Event,
             when SENDING              => Stuck_Watch_Text.Queued_In_Call,
             when RECEIVING            => Stuck_Watch_Text.Idle_Receive,
             when WAITINGFORREPLY      => Stuck_Watch_Text.Waiting_For_Reply,
             when WAITINGFORCOMPLETION => Stuck_Watch_Text.Waiting_For_Completion,
             when SUSPENDED            => Stuck_Watch_Text.Suspended,
             when FUTEXWAITING         => Stuck_Watch_Text.Futex_Waiting,
             when VMHALTED             => Stuck_Watch_Text.Guest_Halted);

        function stateName (s : ProcessState) return String is
          (Stuck_Watch_Text.State_Word (stateClass (s)));

        why : Stuck_Watch_Text.Reasons;
        silenceMs : constant Unsigned_64 := Time.Boot_CPU_Silence_Ms;
    begin
        --  The latest kernel verdicts (noteEvidence), while recent.
        Spinlocks.enterCriticalSection (evidenceLock);
        if evidenceCount > 0 and then nowMs >= evidenceAtMs
          and then nowMs - evidenceAtMs < Evidence_Show_Ms
        then
            for i in 1 .. evidenceCount loop
                count := count + 1;
                lines (count) := evidenceText (i);
            end loop;
            why.Kernel_Evidence := True;
        end if;
        Spinlocks.exitCriticalSection (evidenceLock);
        --  CPU 0 taking no timer work while another CPU keeps the time
        --  (Time.clockTick): on screen too, where the log may be out of reach.
        if silenceMs > 0 then
            declare
                last : Natural := 0;
            begin
                count := count + 1;
                why.CPU0_Silent := True;
                add (lines (count), last, "CPU 0 ran no timer work for ");
                addNumber (lines (count), last, silenceMs / 1_000);
                add (lines (count), last, "s");
            end;
        end if;
        for tid in ThreadID range 1 .. ThreadID'Last loop
            exit when count = Boot_Diagnostics.Stuck_Rows;
            if Thread_Table.Present (Natural (tid)) then
                declare
                    t      : constant Thread_Table.Element_Ref := threadtab (tid);
                    state  : constant ProcessState := t.state;
                    server : constant ProcessID := t.callTarget;
                    start  : constant Unsigned_64 := t.callStartMs;
                    last   : Natural := 0;
                begin
                    if (state = SENDING or else state = WAITINGFORREPLY)
                      and then start <= nowMs
                      and then nowMs - start >= Stuck_Call_Ms
                    then
                        count := count + 1;
                        why.Stuck_Call := True;
                        addName (lines (count), last, t.process);
                        add (lines (count), last, " -> ");
                        if server = NO_PROCESS then
                            add (lines (count), last, "?");
                        else
                            addName (lines (count), last, server);
                        end if;
                        add (lines (count), last, " ");
                        addNumber (lines (count), last, (nowMs - start) / 1_000);
                        add (lines (count), last, "s ");
                        add (lines (count), last,
                             (if state = SENDING then "queued" else "taken"));
                        if server /= NO_PROCESS then
                            add (lines (count), last, " srv:");
                            add (lines (count), last, stateName (threadOf (server).state));
                            if threadOf (server).state in RUNNING | READY then
                                add (lines (count), last, " rip=");
                                addHex (lines (count), last, threadOf (server).sampledRIP);
                            end if;
                        end if;
                        --  Serial too, for headless runs: the output lock
                        --  disables interrupts and nests on this CPU.
                        TextIO.println ("STUCK-CALL: " & lines (count) (1 .. last));
                    end if;
                end;
            end if;
        end loop;
        --  The watch: the desktop's graphics chain, when one of its threads
        --  is hot for Watch_Hot_Reports reports or a main thread sits in a
        --  futex or completion wait undispatched for Stuck_Call_Ms.
        declare
            periodMs  : constant Unsigned_64 :=
              (if nowMs > lastReportMs then nowMs - lastReportMs else 1);
            ticksPerMs : constant Unsigned_64 := Unsigned_64 (Time.tscFrequencyHz) / 1_000;
            suspicious : Boolean := False;

            function isWatched (pid : ProcessID) return Boolean is
                name : constant ProcessName := proctab (pid).name;
            begin
                for w of Watched_Names loop
                    if name (1 .. w'Length) = w then return True; end if;
                end loop;
                return False;
            end isWatched;
        begin
            for tid in ThreadID range 1 .. ThreadID'Last loop
                watchSamples (tid).watched := False;
                watchSamples (tid).firedHot := False;
                watchSamples (tid).firedQuiet := False;
                if Thread_Table.Present (Natural (tid)) and then
                   threadtab (tid).process /= NO_PROCESS and then
                   isWatched (threadtab (tid).process)
                then
                    declare
                        t : constant Thread_Table.Element_Ref := threadtab (tid);
                        w : Watch_Sample renames watchSamples (tid);
                        residency : constant Unsigned_64 := t.execution.Residency_Ticks;
                        dispatches : constant Unsigned_64 :=
                          t.execution.Scheduled_Dispatches + t.execution.Direct_Dispatches;
                        used : constant Unsigned_64 :=
                          (if residency >= w.residency then residency - w.residency else 0);
                    begin
                        w.watched := True;
                        w.percent := (if ticksPerMs > 0
                                      then Unsigned_64'Min (999, used * 100 / (ticksPerMs * periodMs))
                                      else 0);
                        if dispatches /= w.dispatches or else w.quietSince = 0 then
                            w.quietSince := nowMs;
                        end if;
                        w.hotReports := (if w.percent >= Watch_Hot_Percent
                                         then Natural'Min (w.hotReports + 1, 1_000) else 0);
                        w.residency := residency;
                        w.dispatches := dispatches;
                        w.firedHot := w.hotReports >= Watch_Hot_Reports;
                        w.firedQuiet :=
                          tid = mainThreadOf (t.process) and then
                          t.state in FUTEXWAITING | WAITINGFORCOMPLETION | WAITING and then
                          nowMs - w.quietSince >= Stuck_Call_Ms;
                        why.Hot_Thread := why.Hot_Thread or else w.firedHot;
                        why.Quiet_Main := why.Quiet_Main or else w.firedQuiet;
                        if w.firedHot or else w.firedQuiet then
                            suspicious := True;
                        end if;
                    end;
                end if;
            end loop;
            if suspicious or else count > 0 then
                --  Why the watch printed, first: the rows below are context.
                if count < Boot_Diagnostics.Stuck_Rows then
                    declare
                        last : Natural := 0;
                    begin
                        count := count + 1;
                        add (lines (count), last, "why:");
                        add (lines (count), last, Stuck_Watch_Text.Reason_Words (why));
                        TextIO.println ("STUCK-WATCH: " & lines (count) (1 .. last));
                    end;
                end if;
                for tid in ThreadID range 1 .. ThreadID'Last loop
                    exit when count = Boot_Diagnostics.Stuck_Rows;
                    if watchSamples (tid).watched then
                        declare
                            t : constant Thread_Table.Element_Ref := threadtab (tid);
                            last : Natural := 0;
                        begin
                            count := count + 1;
                            add (lines (count), last, "* ");
                            addName (lines (count), last, t.process);
                            add (lines (count), last, " t");
                            addNumber (lines (count), last, Unsigned_64 (tid));
                            add (lines (count), last, " ");
                            add (lines (count), last, stateName (t.state));
                            add (lines (count), last, " cpu ");
                            addNumber (lines (count), last, watchSamples (tid).percent);
                            add (lines (count), last, "% quiet ");
                            addNumber (lines (count), last,
                                       (nowMs - watchSamples (tid).quietSince) / 1_000);
                            add (lines (count), last, "s rip=");
                            addHex (lines (count), last, t.sampledRIP);
                            add (lines (count), last, Stuck_Watch_Text.Fired_Words
                                 (watchSamples (tid).firedHot, watchSamples (tid).firedQuiet));
                            --  Its current or last call: whom, and which request.
                            if t.callTarget /= NO_PROCESS then
                                add (lines (count), last, Stuck_Watch_Text.Call_Words (stateClass (t.state)));
                                addName (lines (count), last, t.callTarget);
                                add (lines (count), last, " lbl=");
                                addShortHex (lines (count), last, t.sendMsg.tag.label);
                            end if;
                            TextIO.println ("STUCK-WATCH: " & lines (count) (1 .. last));
                        end;
                    end if;
                end loop;
            end if;
        end;
        lastReportMs := nowMs;
        Boot_Diagnostics.Stuck_Calls (lines, count);
    end reportStuckCalls;

end Process.Stuck_Calls;
