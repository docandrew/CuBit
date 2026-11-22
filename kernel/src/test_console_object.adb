pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Process.User_Memory;
with TextIO;
with Test_Console_ABI;

package body Test_Console_Object is
    package ABI renames Test_Console_ABI;

    procedure Call (Caller   : Process.ProcessID;
                    Writable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message)
    is
        Asked : constant ABI.Decision :=
          ABI.Decide (Writable, Unsigned_64 (Request.tag.label),
                      Unsigned_64 (Request.tag.length),
                      Request.words (ABI.Count_Word));
        Result : ABI.Outcome := ABI.Done;
        Text   : String (1 .. ABI.Maximum_Bytes);
        Copied : Boolean;
    begin
        if Asked.Valid then
            -- Copied first, so a fault on the caller's buffer never happens
            -- under TextIO's output lock; printed whole, as one string.
            Process.User_Memory.Copy
              (Caller, Request.words (ABI.Address_Word), Text'Address,
               Storage_Count (Asked.Count), Copied);
            if Copied then
                TextIO.printUser (Text (1 .. Natural (Asked.Count)));
            else
                Result := ABI.Unreadable;
            end if;
        else
            Result := Asked.Why;
        end if;
        Reply := Process.NULL_MESSAGE;
        Reply.tag.label := Request.tag.label;
        Reply.tag.length := ABI.Reply_Words;
        Reply.words (ABI.Outcome_Word) := ABI.Outcome_Word_Of (Result);
    end Call;
end Test_Console_Object;
