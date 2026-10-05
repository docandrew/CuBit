with CuBit.Failures;

package body CCL.Host_Replay with SPARK_Mode => On is
   use type Interfaces.Unsigned_32;
   use type CCL.Host_Values.Value;

   procedure Rewind (Item : in out Log) is
   begin
      Item.Next := 0;
      Item.Divergence := False;
   end Rewind;

   procedure Clear (Item : in out Context) is
   begin
      Item.Calls.Next := 0;
      Item.Calls.Count := 0;
      Item.Calls.Overflow := False;
      Item.Calls.Divergence := False;
   end Clear;

   procedure Invoke_Logged
     (Item : in out Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      L : Log renames Item.Calls;
   begin
      if L.Next < L.Count then
         declare
            Logged : Call renames L.Calls (L.Next + 1);
         begin
            if Logged.Binding = Binding and then Logged.Argument = Argument then
               Reply := Logged.Reply;
               L.Next := L.Next + 1;
            else
               L.Divergence := True;
               Reply := (Value => <>, Success => False,
                         Why => CuBit.Failures.Failed
                           (CuBit.Failures.Refused, "the entry changed while it waited for a task",
                            "submit it again"));
            end if;
         end;
         return;
      end if;
      Invoke (Item.Inner, Binding, Argument, Reply);
      if L.Count < MAX_CALLS then
         L.Calls (L.Count + 1) := (Binding => Binding, Argument => Argument, Reply => Reply);
         L.Count := L.Count + 1;
         L.Next := L.Count;
      else
         L.Overflow := True;
      end if;
   end Invoke_Logged;

   procedure Read_Logged
     (Item : in out Context; Request : CCL.Streams.View_Request;
      Reply : in out CCL.Streams.View_Reply) is
   begin
      Read_Stream (Item.Inner, Request, Reply);
   end Read_Logged;
end CCL.Host_Replay;
