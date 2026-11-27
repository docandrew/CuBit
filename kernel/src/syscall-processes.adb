-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall process creation: SPAWN (moved from Syscall.IPC, IPC revision
-- 2 S0).
-------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Boot_Timeline;
with Capabilities;
with Capabilities.Operations;
with Capability_ABI;
with Process_Capabilities;
with ELF;
with Process;
with Process_Identities;
with Process.Loader;
with Process.User_Memory;
with Spawn_Admission;
with Spinlocks;
with TextIO; use TextIO;
with Util;
with User_Buffer_Copy;
with Syscall.Lookups; use Syscall.Lookups;

use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall.Processes is

    procedure handleSpawn (callerPID : Process.ProcessID;
                           arg0, arg1, arg2 : Unsigned_64;
                           retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- Raw syscall/process integration; pure helpers proved separately
    is
        package ABI renames Capability_ABI;
        request  : aliased ABI.Spawn_Request := [others => 0];
        result   : aliased ABI.Spawn_Result := [others => 0];
        elfAddr  : System.Address;
        elfSize  : Storage_Count;
        elfHeader : aliased ELF.ELFFileHeader;
        name : aliased Process.ProcessName := [others => ASCII.NUL];
        copied : Boolean;
        newPID   : Process.ProcessID;
        parent   : Process.ProcessID := callerPID;
        limit    : Process_Capabilities.Tables.Handle_Limit;
        limitOK  : Boolean;
        started  : constant Unsigned_64 := Boot_Timeline.Now;
        refuse   : ABI.Refusal := ABI.Invalid_Argument;
        found    : Process_Capabilities.Found;

        procedure refused (why : ABI.Refusal; text : String) is
        begin
            println (text);
            retval := ABI.Refusal_Word (why);
        end refused;
    begin
        retval := ABI.Refusal_Word (ABI.Invalid_Argument);

        -- arg0 names the spawn authority: a CAP_PROCESS with RIGHT_EXECUTE
        -- that names every process (Spawn_Admission rule 1). A child's
        -- Control never spawns: it would mint control of another process.
        found := findOwn (callerPID, arg0, Capabilities.CAP_PROCESS,
                          onlyRight (Capabilities.RIGHT_EXECUTE));
        if not found.OK then
            refused (ABI.Not_Permitted, "SPAWN: denied, no CAP_PROCESS/EXECUTE");
            return;
        elsif not Spawn_Admission.Is_Spawn_Authority (found.Cap) then
            refused (ABI.Not_Permitted, "SPAWN: denied, the authority names one process");
            return;
        end if;
        Process.User_Memory.Copy
          (callerPID, arg1, request'Address, ABI.Spawn_Request_Bytes, copied);
        if not copied or else
           not Process.User_Memory.Writable_Range (callerPID, arg2, ABI.Spawn_Result_Bytes)
        then
            refused (refuse, "SPAWN: request or result unreadable");
            return;
        end if;

        -- The parent by identity; a stale one refuses the spawn rather than
        -- naming whoever holds the slot now. Another process than the caller
        -- only under Grant over it (rule 3): the parent takes the child's
        -- exit and may send it control messages. Checked before the image.
        if request (ABI.Parent) /= 0 then
            parent := Process.processOfIdentity (Process_Identities.From_Word (request (ABI.Parent)));
            if parent = Process.NO_PROCESS then
                refused (ABI.Target_Gone, "SPAWN: parent identity is stale");
                return;
            elsif not Spawn_Admission.Parent_Admitted
                (found.Cap, parent = callerPID, Unsigned_64 (parent),
                 Process.generationOf (parent))
            then
                refused (ABI.Not_Permitted, "SPAWN: denied, no Grant over the named parent");
                return;
            end if;
        end if;
        Process_Capabilities.Limit_Of
          (request (ABI.Limit), request (ABI.Limit_Count), limit, limitOK);
        if not limitOK then
            refused (refuse, "SPAWN: invalid handle limit");
            return;
        elsif request (ABI.Image_Bytes) < ELF.ELFFileHeader'Size / 8 or else
          not User_Buffer_Copy.Valid_Range (request (ABI.Image), request (ABI.Image_Bytes))
        then
            refused (refuse, "SPAWN: invalid ELF source range");
            return;
        end if;
        elfAddr := Util.numToAddr (request (ABI.Image));
        -- Validate before converting the unsigned ABI length or reading it.
        elfSize := Storage_Count (request (ABI.Image_Bytes));
        Process.User_Memory.Copy
          (callerPID, request (ABI.Image), elfHeader'Address, ELF.ELFFileHeader'Size / 8, copied);
        if not copied then
            refused (refuse, "SPAWN: ELF header source unreadable");
            return;
        end if;

        if not Process.Loader.isValidELF (elfHeader) then
            refused (refuse, "SPAWN: invalid ELF header");
            return;
        end if;

        -- The name (0 = default "spawned").
        if request (ABI.Name) /= 0 then
            Process.User_Memory.Copy_Name (callerPID, request (ABI.Name), name, copied);
            if not copied then
                refused (refuse, "SPAWN: process name unreadable");
                return;
            end if;
        else
            name (1 .. 7) := "spawned";
        end if;

        newPID := Process.Loader.load (
            elfHeader    => elfHeader,
            objStart     => elfAddr,
            size         => elfSize,
            strAddr      => name'Address,
            requestedPID => Process.NO_PROCESS,
            ppid         => parent,
            sourcePID    => callerPID,
            handleLimit  => limit);

        if newPID = Process.NO_PROCESS then
            refused (ABI.Out_Of_Memory, "SPAWN: load failed");
            return;
        end if;

        -- The spawner's handles for the new process, kernel objects for
        -- their owner, never wider than the authority (Spawn_Admission rule
        -- 2): control with the authority's rights; with Grant, also an
        -- untagged endpoint it may badge and pass on and the CSPACE for
        -- installing the child's startup capabilities until it first runs
        -- (ABI.No_Handle otherwise). Placed under the spawner's mailbox lock.
        declare
            gen : constant Capabilities.Generation := Process.generationOf (newPID);
            ref : constant Unsigned_64 := Unsigned_64 (newPID);
            mailbox : constant Boolean := Spawn_Admission.Gives_Mailbox (found.Cap);
            words : array (1 .. 3) of Unsigned_64 := [others => ABI.No_Handle];
        begin
            Spinlocks.enterCriticalSection (Process.tableLock (callerPID));
            Process_Capabilities.Insert
              (Process.proctab(callerPID).caps,
               (capType => Capabilities.CAP_PROCESS,
                rights  => Spawn_Admission.Control_Rights (found.Cap),
                authorityTag => Capabilities.NO_AUTHORITY_TAG,
                object  => (ref => ref, param => 0),
                gen     => gen), words (1));
            if mailbox then
                Process_Capabilities.Insert
                  (Process.proctab(callerPID).caps,
                   (capType => Capabilities.CAP_ENDPOINT,
                    rights  => Spawn_Admission.Endpoint_Rights (found.Cap),
                    authorityTag => Capabilities.NO_AUTHORITY_TAG,
                    object  => (ref => ref, param => 0),
                    gen     => gen), words (2));
                Process_Capabilities.Insert
                  (Process.proctab(callerPID).caps,
                   (capType => Capabilities.CAP_CSPACE,
                    rights  => Spawn_Admission.Cspace_Rights,
                    authorityTag => Capabilities.NO_AUTHORITY_TAG,
                    object  => (ref => ref, param => 0),
                    gen     => gen), words (3));
            end if;
            if not ABI.Is_Handle_Word (words (1)) or else
               (mailbox and then
                not (ABI.Is_Handle_Word (words (2)) and then ABI.Is_Handle_Word (words (3))))
            then
                -- The spawner's table is full: undo, and discard the child.
                for W of words loop
                    if W = ABI.No_Handle then
                        null;
                    elsif ABI.Is_Handle_Word (W) then
                        declare
                            ignoreCap  : Capabilities.Capability;
                            ignoreWord : Unsigned_64;
                        begin
                            Process_Capabilities.Close
                              (Process.proctab(callerPID).caps, W, ignoreCap, ignoreWord);
                        end;
                    else
                        refuse := ABI.To_Refusal (W);
                    end if;
                end loop;
                Spinlocks.exitCriticalSection (Process.tableLock (callerPID));
                -- Published and suspended, never run: stop it (the reaper
                -- releases it).
                if not Process.killProcess (newPID, Process.generationOf (newPID)) then
                    println ("SPAWN: rollback stop refused");
                end if;
                refused (refuse, "SPAWN: no room for the spawner's handles");
                return;
            end if;
            Spinlocks.exitCriticalSection (Process.tableLock (callerPID));
            result :=
              [ABI.Child              => Process_Identities.To_Word (Process.identityOf (newPID)),
               ABI.Process            => words (1),
               ABI.Endpoint           => words (2),
               ABI.Cspace             => words (3),
               ABI.Child_Self         => Process.proctab(newPID).selfHandle,
               ABI.Child_Self_Process => Process.proctab(newPID).selfProcessHandle];
        end;

        Boot_Timeline.Note_Spawn
          (Slot     => Natural (newPID),
           Identity => result (ABI.Child),
           Spawner  => Process_Identities.To_Word (Process.identityOf (callerPID)),
           Spawner_Slot => Natural (callerPID),
           Name     => name,
           Started  => started);
        Process.User_Memory.Copy_To_User
          (callerPID, arg2, result'Address, ABI.Spawn_Result_Bytes, copied);
        -- Always spawn suspended; the spawner installs and resumes.
        print ("SPAWN: created suspended PID ");
        println (Integer (newPID));
        retval := (if copied then 0 else ABI.Refusal_Word (ABI.Invalid_Argument));
    end handleSpawn;

end Syscall.Processes;
