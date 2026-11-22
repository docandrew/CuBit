pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

with Interfaces; use Interfaces;

with acpi;
with Firmware_Tables;
with Firmware_Tables.Snapshots;
with Firmware_Tables.Snapshots.Transfers;
with Firmware_Snapshot_Service;
with Process.User_Memory;

package body Firmware_Snapshot_Export is

    package ABI renames Firmware_Snapshot_ABI;
    use type Firmware_Tables.Snapshots.Phase;
    use type ABI.Outcome;

    function Snapshot_Ready return Boolean is
        (acpi.Table_Snapshot_State = Firmware_Tables.Snapshots.Ready);

    procedure Call
      (Caller   : Process.ProcessID;
       Readable : Boolean;
       Request  : Process.Message;
       Reply    : out Process.Message)
    is
        Got : ABI.Reply := ABI.Empty_Reply;

        procedure Write_User
          (Destination : Unsigned_64; Source : Firmware_Tables.Bytes; Success : out Boolean) is
        begin
            Process.User_Memory.Copy_To_User
              (Caller, Destination, Source'Address, Storage_Count (Source'Length), Success);
        end Write_User;

        -- The reply goes back in the message (Payload_Of), not to memory.
        procedure Write_Reply (Item : ABI.Reply; Success : out Boolean) is
        begin
            Got := Item;
            Success := True;
        end Write_Reply;

        package Service is new Firmware_Snapshot_Service
          (Snapshot_Ready => Snapshot_Ready,
           Table_Count    => acpi.Snapshot_Table_Count,
           Describe       => acpi.Describe_Snapshot_Table,
           Copy_Part      => acpi.Copy_Table_Part,
           Write_User     => Write_User,
           Write_Reply    => Write_Reply);

        Words : constant ABI.Request_Words :=
          (Op          => Unsigned_64 (Request.tag.label),
           Table       => Request.words (0),
           Offset      => Request.words (1),
           Destination => Request.words (2),
           Capacity    => Request.words (3));
        Outcome : ABI.Outcome;
    begin
        -- The authority first; nothing in the request is read before it.
        if not Readable then
            Outcome := ABI.Not_Authorized;
        elsif Request.tag.length /= ABI.Message_Words then
            Outcome := ABI.Malformed;
        else
            Service.Serve (Words, Outcome);
        end if;
        Reply := Process.NULL_MESSAGE;
        Reply.tag.label := Unsigned_32 (ABI.Outcome_Word (Outcome));
        Reply.tag.length := ABI.Message_Words;
        if Outcome = ABI.Ok then
            declare
                Payload : constant ABI.Message_Payload :=
                  ABI.Payload_Of (ABI.Operation_Of (Words.Op), Got);
            begin
                for I in ABI.Message_Index loop
                    Reply.words (Natural (I)) := Payload (I);
                end loop;
            end;
        end if;
    end Call;

end Firmware_Snapshot_Export;
