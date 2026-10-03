with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Text_To_Log;
with Compositor_Requests;
package body Desktop_Logs is
   use Interfaces;
   package L renames CuBit.Log_Records;
   package T renames CuBit.Text_To_Log;
   use type T.Step_Kind;
   Writer : CuBit.Logging.Publisher;
   Framer : T.Adapter;
   Capacity : constant := 32;
   Queue : array (Natural range 0 .. Capacity - 1) of L.Log_Record;
   Head : Natural range 0 .. Capacity - 1 := 0;
   Used : Natural range 0 .. Capacity := 0;
   Lost, Reported, Active : Unsigned_64 := 0;
   Disabled : Boolean := False;
   procedure Drop is
   begin
      if Lost < Unsigned_64'Last then Lost := Lost + 1; end if;
   end Drop;
   procedure Write (Text : String) is
      Step : T.Step;
   begin
      CuBit.Messages.debugPrint (Text);
      if Disabled then return; end if;
      for C of Text loop
         T.Feed (Framer, C, (Clock => L.Unspecified), Step);
         case T.Kind (Step) is
            when T.Record_Ready =>
               if Used = Capacity then Drop;
               else
                  Queue ((Head + Used) mod Capacity) := T.Value (Step);
                  Used := Used + 1;
               end if;
            when T.Line_Dropped => Drop;
            when T.Need_More => null;
         end case;
      end loop;
   end Write;
   function Matches (Token : Unsigned_64) return Boolean is
     (Active /= 0 and then Token = Active);
   procedure Collect (Completion : CuBit.Messages.CompletionEntry) is
      Handled : Boolean;
   begin
      CuBit.Logging.Complete (Writer, Completion, Handled);
      if Handled then Active := 0; end if;
   end Collect;
   procedure Pump (Sequence : in out Unsigned_64) is
      Token : Unsigned_64;
      Accepted : Boolean;
      Value : L.Log_Record;
   begin
      if Disabled or else CuBit.Logging.Pending (Writer) then return; end if;
      if Used = 0 then
         if Lost = Reported then return; end if;
         declare R : constant L.Decoded := L.Make
           ("desktop: log records dropped=" & Lost'Image &
            " publisher_dropped=" & CuBit.Logging.Dropped (Writer)'Image,
            L.Warning);
         begin
            if not R.Success then return; end if;
            Value := R.Value;
         end;
         Reported := Lost;
      else
         Value := Queue (Head);
         Head := (Head + 1) mod Capacity;
         Used := Used - 1;
      end if;
      Compositor_Requests.Allocate (Sequence, Token);
      if Token = 0 then Disabled := True; return; end if;
      CuBit.Logging.Emit (Writer, Value, Token, Accepted);
      if Accepted then Active := Token;
      else Disabled := True; Used := 0;
      end if;
   end Pump;
end Desktop_Logs;
