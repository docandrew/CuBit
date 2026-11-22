pragma Ada_2022;
with Interfaces; use Interfaces;
with Build;
with Trace;
with Trace_ABI;

package body Trace_Object is
    package ABI renames Trace_ABI;

    procedure Call (Writable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message)
    is
        Asked : constant ABI.Decision :=
          ABI.Decide (Writable, Unsigned_64 (Request.tag.label),
                      Unsigned_64 (Request.tag.length), Build.Test_Latency_Trace);
        Result : ABI.Outcome := ABI.Done;
    begin
        if Asked.Valid then
            case Asked.Op is
                when ABI.Reset        => Trace.Reset;
                when ABI.Summary      =>
                    Trace.PrintSummary;
                    Process.printOwnAccounting;
                when ABI.Start_Local  => Trace.StartLocal;
                when ABI.Freeze_Local => Trace.FreezeLocal;
                when ABI.Dump_Local   => Trace.DumpLocal;
            end case;
        else
            Result := Asked.Why;
        end if;
        Reply := Process.NULL_MESSAGE;
        Reply.tag.label := Request.tag.label;
        Reply.tag.length := ABI.Reply_Words;
        Reply.words (ABI.Outcome_Word) := ABI.Outcome_Word_Of (Result);
    end Call;
end Trace_Object;
