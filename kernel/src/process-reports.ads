-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Exit and fault reports (moved from Process.IPC, IPC revision 2 S0): the
-- kernel's reports about a process to its parent, its manager and its
-- supervisor, kept in Kernel_Reports until read, and the PID retention
-- that waits for the last read. Process.IPC's notice lane delivers them.
-------------------------------------------------------------------------------
package Process.Reports is

    -- pid's exit report, kept for its parent and for the process manager
    -- (their lives as given); ring* say whom to ring (ringReport) once the
    -- caller's locks are released. Before releaseRetiredPID.
    procedure reportExit
      (pid : ProcessID; msg : Message;
       parent : ProcessID; parentGeneration : Capabilities.Generation;
       manager : ProcessID; managerGeneration : Capabilities.Generation;
       ringParent, ringManager : out Boolean);

    -- A retired, invalidated pid goes back to the allocator once no report
    -- about it is unread; until then its release waits for the last read.
    procedure releaseRetiredPID (pid : ProcessID);

    ---------------------------------------------------------------------------
    -- notifySupervisor
    -- Send a non-blocking fault event to the supervisor of the given process.
    -- Safe to call before acquiring Process.lock (respects lock ordering).
    -- @param pid        - offending process
    -- @param faultLabel - EVENT_CAP_FAULT or EVENT_PROCESS_FAULT
    -- @param detail0    - syscall number or exception vector
    -- @param detail1    - what was attempted (arg0 or fault address)
    -- @param detail2    - additional context (arg1 or RIP)
    ---------------------------------------------------------------------------
    procedure notifySupervisor (pid        : ProcessID;
                                faultLabel : Unsigned_32;
                                detail0    : Unsigned_64;
                                detail1    : Unsigned_64;
                                detail2    : Unsigned_64);

    -- The notice lane's side (Process.IPC): whether receiver has an unread
    -- report; take its next one into msg (msg is unchanged when found is
    -- False); open the store for pid's current life; close it at
    -- reclamation, freeing PIDs that waited only for pid's reading. Each
    -- takes reportLock, a leaf.
    function anyUnread (receiver : ProcessID) return Boolean;
    procedure takeReport
      (receiver : ProcessID; msg : in out Message; found : out Boolean);
    procedure openReports (pid : ProcessID);
    procedure closeReports (pid : ProcessID);

end Process.Reports;
