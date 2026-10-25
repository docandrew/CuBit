pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

with acpi;
with Capabilities;
with Firmware_Tables;
with Firmware_Tables.Snapshots;
with Firmware_Tables.Snapshots.Transfers;
with Firmware_Snapshot_Service;
with IPC_Labels;
with Process.IPC;
with Process.User_Memory;
with Process_Capabilities;
with Spinlocks;

package body Firmware_Snapshot_Export is

    package ABI renames Firmware_Snapshot_ABI;
    use type Firmware_Tables.Snapshots.Phase;

    -- The capability Handle_Word names in Caller's own table, checked under
    -- its mailbox lock (CAP-002: authority is named by handle only).
    function Authorized (Caller : Process.ProcessID; Handle_Word : Unsigned_64)
      return Boolean
    is
        Found : Process_Capabilities.Found;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab (Caller).lock);
        Found := Process_Capabilities.Find
          (Process.proctab (Caller).caps, Handle_Word,
           Capabilities.CAP_FIRMWARE_SNAPSHOT, Capabilities.READ_ONLY);
        Spinlocks.exitCriticalSection (Process.mailtab (Caller).lock);
        return Found.OK;
    end Authorized;

    function Snapshot_Ready return Boolean is
        (acpi.Table_Snapshot_State = Firmware_Tables.Snapshots.Ready);

    procedure Handle
      (Caller      : Process.ProcessID;
       Handle_Word : Unsigned_64;
       Words       : Firmware_Snapshot_ABI.Request_Words;
       Reply       : Unsigned_64;
       Result      : out Unsigned_64)
    is
        procedure Write_User
          (Destination : Unsigned_64; Source : Firmware_Tables.Bytes; Success : out Boolean) is
        begin
            Process.User_Memory.Copy_To_User
              (Caller, Destination, Source'Address, Storage_Count (Source'Length), Success);
        end Write_User;

        procedure Write_Reply (Item : ABI.Reply; Success : out Boolean) is
            Copy : aliased constant ABI.Reply := Item;
        begin
            Process.User_Memory.Copy_To_User
              (Caller, Reply, Copy'Address, ABI.Reply_Bytes, Success);
        end Write_Reply;

        package Service is new Firmware_Snapshot_Service
          (Snapshot_Ready => Snapshot_Ready,
           Table_Count    => acpi.Snapshot_Table_Count,
           Describe       => acpi.Describe_Snapshot_Table,
           Copy_Part      => acpi.Copy_Table_Part,
           Write_User     => Write_User,
           Write_Reply    => Write_Reply);

        Outcome : ABI.Outcome;
    begin
        -- The authority first, against the actual caller; nothing in the
        -- request is read before it.
        if not Authorized (Caller, Handle_Word) then
            Process.IPC.notifySupervisor
              (Caller, IPC_Labels.EVENT_CAP_FAULT, ABI.System_Call_Number, Handle_Word, Words.Op);
            Result := ABI.Outcome_Word (ABI.Not_Authorized);
            return;
        end if;
        Service.Serve (Words, Outcome);
        Result := ABI.Outcome_Word (Outcome);
    end Handle;

end Firmware_Snapshot_Export;
