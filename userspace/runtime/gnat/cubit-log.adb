pragma Ada_2022;
with CuBit.Logging;
with CuBit.Messages;

package body CuBit.Log is
   use type Logs.Severity;

   Writer : CuBit.Logging.Publisher;
   Echo : Boolean := True;
   --  Records that could not be made (too long, bad text); the ring's own
   --  sheds are the Writer's (CuBit.Logging.Dropped).
   Losses : Unsigned_64 := 0;

   procedure Lose;
   procedure Lose is
   begin
      if Losses < Unsigned_64'Last then
         Losses := Losses + 1;
      end if;
   end Lose;

   function Wanted (Level : Logs.Severity) return Boolean is
     (CuBit.Logging.Wanted (Writer, Level));

   procedure Set_Echo (Enabled : Boolean) is
   begin
      Echo := Enabled;
   end Set_Echo;

   procedure Write (Level : Logs.Severity; Text : String) is
      Made : Logs.Decoded;
      Accepted : Boolean;
   begin
      if Echo then
         CuBit.Messages.debugPrint (Text & ASCII.LF);
      end if;
      if not Wanted (Level) then
         return;
      end if;
      Made := Logs.Make (Text, Level);
      if not Made.Success then
         Lose;
      else
         --  A copy into the ring: no IPC, no waiting (sheds count in Writer).
         CuBit.Logging.Emit (Writer, Made.Value, Accepted);
      end if;
   end Write;

   procedure Trace (Text : String) is
   begin
      Write (Logs.Trace, Text);
   end Trace;
   procedure Debug (Text : String) is
   begin
      Write (Logs.Debug, Text);
   end Debug;
   procedure Info (Text : String) is
   begin
      Write (Logs.Information, Text);
   end Info;
   procedure Warning (Text : String) is
   begin
      Write (Logs.Warning, Text);
   end Warning;
   procedure Error (Text : String) is
   begin
      Write (Logs.Error, Text);
   end Error;
   procedure Critical (Text : String) is
   begin
      Write (Logs.Critical, Text);
   end Critical;

   procedure Flush is
      Drained : Boolean;
   begin
      CuBit.Logging.Flush (Writer, Drained);
   end Flush;

   function Lost return Unsigned_64 is (Losses + CuBit.Logging.Dropped (Writer));
end CuBit.Log;
