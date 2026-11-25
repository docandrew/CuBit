pragma Ada_2022;
with Interfaces; use Interfaces;
with Process_Identities;
with Process.User_Memory;
with Process_List_ABI;

package body Process_List_Object is
    package ABI renames Process_List_ABI;
    use type Process.ProcessState;
    use type Process.LatencyClass;
    use type ABI.Outcome;

    -- One entry, as CuBit.Process_List decodes it.
    type Proc_Entry is record
        identity : Unsigned_64;
        state    : Unsigned_8;
        cpu      : Unsigned_8;
        class    : Unsigned_8;
        reserved : Unsigned_8;
        frames   : Unsigned_32;
        name     : String (1 .. 16);
    end record;
    for Proc_Entry use record
        identity at 0 range 0 .. 63;
        state    at 8 range 0 .. 7;
        cpu      at 9 range 0 .. 7;
        class    at 10 range 0 .. 7;
        reserved at 11 range 0 .. 7;
        frames   at 12 range 0 .. 31;
        name     at 16 range 0 .. 127;
    end record;
    for Proc_Entry'Size use ABI.Entry_Bytes * 8;

    -- The class its main thread runs in (Process.runKeyOf's bands).
    function Class_Of (P : Process.ProcessID) return ABI.Scheduling_Class is
      (if Process.threadOf (P).idle then ABI.Idle
       elsif Process.threadOf (P).rtAdmitted and then
         Process.threadOf (P).latency.class = Process.LATENCY_REALTIME then ABI.Realtime
       elsif Process.threadOf (P).latency.class = Process.LATENCY_BACKGROUND then ABI.Background
       else ABI.Normal);

    procedure Call (Caller   : Process.ProcessID;
                    Readable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message)
    is
        Result : ABI.Outcome :=
          ABI.Refusal (Readable, Unsigned_64 (Request.tag.label), Unsigned_64 (Request.tag.length));
        Destination : constant Unsigned_64 := Request.words (ABI.Destination_Word);
        Maximum : constant Unsigned_64 := ABI.Entries_In (Request.words (ABI.Capacity_Word));
        Count : Unsigned_64 := 0;
    begin
        if Result = ABI.Listed then
            -- Each entry is built here and copied to the caller's buffer
            -- through Process.User_Memory.
            for I in Process.ProctabRange loop
                exit when Count >= Maximum;
                if Process.threadOf (I).state /= Process.INVALID then
                    declare
                        Item : aliased constant Proc_Entry :=
                          (identity => Process_Identities.To_Word (Process.identityOf (I)),
                           state    => Process.ProcessState'Pos (Process.threadOf (I).state),
                           cpu      => Unsigned_8 (Process.threadOf (I).cpu),
                           class    => ABI.Scheduling_Class'Enum_Rep (Class_Of (I)),
                           reserved => 0,
                           frames   => Unsigned_32 (Process.proctab (I).frames.length),
                           name     => Process.proctab (I).name);
                        Copied : Boolean;
                    begin
                        Process.User_Memory.Copy_To_User
                          (Caller, Destination + Count * ABI.Entry_Bytes, Item'Address,
                           ABI.Entry_Bytes, Copied);
                        if not Copied then
                            Result := ABI.Destination_Unwritable;
                            exit;
                        end if;
                        Count := Count + 1;
                    end;
                end if;
            end loop;
        end if;
        Reply := Process.NULL_MESSAGE;
        Reply.tag.label := Request.tag.label;
        Reply.tag.length := ABI.Reply_Words;
        Reply.words (ABI.Outcome_Word) := ABI.Outcome_Word_Of (Result);
        Reply.words (ABI.Count_Word) := (if Result = ABI.Listed then Count else 0);
    end Call;
end Process_List_Object;
