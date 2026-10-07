with Interfaces;
with CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Text_To_Log;
package body Desktop_Logs is
   use Interfaces;
   package L renames CuBit.Log_Records;
   package T renames CuBit.Text_To_Log;
   use type T.Step_Kind;
   Writer : CuBit.Logging.Publisher;
   Framer : T.Adapter;
   Lost, Reported : Unsigned_64 := 0;
   procedure Drop is
   begin
      if Lost < Unsigned_64'Last then Lost := Lost + 1; end if;
   end Drop;
   procedure Publish (Value : L.Log_Record) is
      Accepted : Boolean;
   begin
      CuBit.Logging.Emit (Writer, Value, Accepted);
      if not Accepted then Drop; end if;
   end Publish;
   procedure Write (Text : String) is
      Step : T.Step;
   begin
      CuBit.Messages.debugPrint (Text);
      for C of Text loop
         T.Feed (Framer, C, (Clock => L.Unspecified), Step);
         case T.Kind (Step) is
            when T.Record_Ready => Publish (T.Value (Step));
            when T.Line_Dropped => Drop;
            when T.Need_More => null;
         end case;
      end loop;
      if Lost /= Reported then
         Reported := Lost;
         declare R : constant L.Decoded := L.Make
           ("desktop: log records dropped=" & Lost'Image, L.Warning);
         begin
            if R.Success then Publish (R.Value); end if;
         end;
      end if;
   end Write;
end Desktop_Logs;
