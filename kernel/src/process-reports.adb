-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Process.Reports - implementation.
-------------------------------------------------------------------------------
with Kernel_Reports;
with Process.IPC;
with Process_Identities;
with Spinlocks;

package body Process.Reports is

    -- Exit and fault reports. reportLock is a leaf.
    reportLock : Spinlocks.spinlock;
    reports    : Kernel_Reports.Table;

    ---------------------------------------------------------------------------
    -- Exit and fault reports (docs/ipc-delivery.md)
    ---------------------------------------------------------------------------

    function toReport (msg : Message) return Kernel_Reports.Report is
      (Label   => msg.tag.label,
       Length  => Kernel_Reports.Word_Count'Min
                    (Natural (msg.tag.length), Kernel_Reports.Report_Words),
       Words   => (msg.words (0), msg.words (1), msg.words (2), msg.words (3)),
       Further => 0);

    function toMessage (value : Kernel_Reports.Report) return Message is
      (tag => (label => value.Label, length => Unsigned_8 (value.Length),
               protocol => 0, ipc => <>, reserved => value.Further),
       authorityTag => 0,
       words => (value.Words (0), value.Words (1), value.Words (2), value.Words (3)));

    -- A retired process whose last report was just read or dropped.
    procedure freeReleasedPID (pid : Kernel_Reports.Process) is
    begin
        if pid /= Kernel_Reports.No_Process then
            PIDTracker.freePID (ProcessID (pid), invalidated => True);
        end if;
    end freeReleasedPID;

    procedure releaseRetiredPID (pid : ProcessID) is
        freeNow : Boolean;
    begin
        Spinlocks.enterCriticalSection (reportLock);
        Kernel_Reports.Request_Free (reports, Kernel_Reports.Process (pid), freeNow);
        Spinlocks.exitCriticalSection (reportLock);
        if freeNow then
            PIDTracker.freePID (pid, invalidated => True);
        end if;
    end releaseRetiredPID;

    -- Keep a report about subject for recipient (this life); True when
    -- kept, so the caller rings the recipient once its locks are released.
    function putReport
      (subject : ProcessID; kind : Kernel_Reports.Report_Kind;
       recipient : ProcessID; generation : Capabilities.Generation;
       msg : Message) return Boolean
    is
        kept : Boolean := False;
    begin
        if subject = NO_PROCESS or else recipient = NO_PROCESS then
            return False;
        end if;
        Spinlocks.enterCriticalSection (reportLock);
        -- An exit is reported once per life: its PID is not reused while
        -- the report is unread, so the slot is free.
        if kind not in Kernel_Reports.Exit_Kind or else
           not reports.Subjects (Kernel_Reports.Process (subject)).Reports (kind).Unread
        then
            Kernel_Reports.Put
              (reports, Kernel_Reports.Process (subject), kind,
               (Id => Kernel_Reports.Process (recipient),
                Generation => Unsigned_64 (generation)),
               toReport (msg), kept);
        end if;
        Spinlocks.exitCriticalSection (reportLock);
        return kept;
    end putReport;

    procedure reportExit
      (pid : ProcessID; msg : Message;
       parent : ProcessID; parentGeneration : Capabilities.Generation;
       manager : ProcessID; managerGeneration : Capabilities.Generation;
       ringParent, ringManager : out Boolean) is
    begin
        ringParent := parentGeneration /= 0 and then
          putReport (pid, Kernel_Reports.Exit_To_Parent, parent, parentGeneration, msg);
        ringManager := manager /= parent and then manager /= pid and then
          putReport (pid, Kernel_Reports.Exit_To_Manager, manager, managerGeneration, msg);
    end reportExit;

    function anyUnread (receiver : ProcessID) return Boolean is
        any : Boolean;
    begin
        Spinlocks.enterCriticalSection (reportLock);
        any := Kernel_Reports.Unread_For (reports, Kernel_Reports.Process (receiver));
        Spinlocks.exitCriticalSection (reportLock);
        return any;
    end anyUnread;

    procedure takeReport
      (receiver : ProcessID; msg : in out Message; found : out Boolean)
    is
        value : Kernel_Reports.Report;
        released : Kernel_Reports.Process;
    begin
        Spinlocks.enterCriticalSection (reportLock);
        Kernel_Reports.Take
          (reports, Kernel_Reports.Process (receiver), value, found, released);
        Spinlocks.exitCriticalSection (reportLock);
        freeReleasedPID (released);
        if found then
            msg := toMessage (value);
        end if;
    end takeReport;

    procedure openReports (pid : ProcessID) is
    begin
        Spinlocks.enterCriticalSection (reportLock);
        Kernel_Reports.Open
          (reports, Kernel_Reports.Process (pid), Unsigned_64 (generationOf (pid)));
        Spinlocks.exitCriticalSection (reportLock);
    end openReports;

    procedure closeReports (pid : ProcessID) is
        released : Kernel_Reports.Process;
    begin
        loop
            Spinlocks.enterCriticalSection (reportLock);
            Kernel_Reports.Close (reports, Kernel_Reports.Process (pid), released);
            Spinlocks.exitCriticalSection (reportLock);
            exit when released = Kernel_Reports.No_Process;
            freeReleasedPID (released);
        end loop;
    end closeReports;

    ---------------------------------------------------------------------------
    -- notifySupervisor
    -- Send a non-blocking fault event to the supervisor of the given process.
    ---------------------------------------------------------------------------
    procedure notifySupervisor (pid        : ProcessID;
                                faultLabel : Unsigned_32;
                                detail0    : Unsigned_64;
                                detail1    : Unsigned_64;
                                detail2    : Unsigned_64)

    is
        svpid : constant ProcessID := proctab(pid).svpid;
        faultMsg : Message := NULL_MESSAGE;
    begin
        if svpid = NO_PROCESS then
            return;
        end if;

        faultMsg.tag := (label  => faultLabel,
                         length => 4,
                         protocol  => 0, ipc => <>,
                         reserved  => 0);
        faultMsg.words (0) := Process_Identities.To_Word (identityOf (pid));
        faultMsg.words (1) := detail0;
        faultMsg.words (2) := detail1;
        faultMsg.words (3) := detail2;

        if putReport (pid, Kernel_Reports.Fault_To_Supervisor, svpid,
                      generationOf (svpid), faultMsg)
        then
            IPC.ringReport (svpid);
        end if;
    end notifySupervisor;

end Process.Reports;
