pragma Ada_2022;
with CuBit.Logging;
with CuBit.Log_Protocol;

package body CuBit.Log is
   use type Logs.Severity;
   use type CuBit.Log_Protocol.Status;

   CAPACITY : constant := 32;
   subtype Queue_Count is Natural range 0 .. CAPACITY;
   subtype Queue_Slot is Natural range 0 .. CAPACITY - 1;
   type Record_Queue is array (Queue_Slot) of Logs.Log_Record;

   Writer : CuBit.Logging.Publisher;
   Queue : Record_Queue;
   Head : Queue_Slot := 0;
   Used : Queue_Count := 0;
   Current_Mode : Delivery := Queued;
   Echo : Boolean := True;
   --  The minimum learned from synchronous replies (queued replies teach Writer).
   Kept_From : Logs.Severity := Logs.Trace;
   Next_Token : Unsigned_64 := TOKEN_BASE;
   Losses : Unsigned_64 := 0;
   Disabled : Boolean := False;

   procedure Lose;
   procedure Publish_Now (Value : Logs.Log_Record);

   procedure Lose is
   begin
      if Losses < Unsigned_64'Last then
         Losses := Losses + 1;
      end if;
   end Lose;

   function Minimum return Logs.Severity is
     (Logs.Severity'Max (Kept_From, CuBit.Logging.Minimum (Writer)));
   function Wanted (Level : Logs.Severity) return Boolean is (Level >= Minimum);

   procedure Set_Delivery (Mode : Delivery) is
   begin
      Current_Mode := Mode;
   end Set_Delivery;

   procedure Set_Echo (Enabled : Boolean) is
   begin
      Echo := Enabled;
   end Set_Echo;

   procedure Publish_Now (Value : Logs.Log_Record) is
      Result : CuBit.Log_Protocol.Status;
      Learned : Logs.Severity;
   begin
      CuBit.Logging.Publish_Now (Value, Result, Learned);
      if Result in CuBit.Log_Protocol.OK | CuBit.Log_Protocol.Below_Minimum then
         Kept_From := Learned;
      else
         Lose;
      end if;
   end Publish_Now;

   procedure Write (Level : Logs.Severity; Text : String) is
      Made : Logs.Decoded;
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
      elsif Current_Mode = Immediate then
         Publish_Now (Made.Value);
      elsif Used = CAPACITY then
         Lose;
      else
         Queue ((Head + Used) mod CAPACITY) := Made.Value;
         Used := Used + 1;
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

   procedure Pump is
      Accepted : Boolean;
   begin
      if Disabled or else Used = 0 or else CuBit.Logging.Pending (Writer) then
         return;
      end if;
      --  A minimum learned since the record was queued still applies.
      while Used > 0 and then not Wanted (Logs.Level (Queue (Head))) loop
         Head := (Head + 1) mod CAPACITY;
         Used := Used - 1;
      end loop;
      if Used = 0 then
         return;
      end if;
      CuBit.Logging.Emit (Writer, Queue (Head), Next_Token, Accepted);
      if Accepted then
         Head := (Head + 1) mod CAPACITY;
         Used := Used - 1;
         Next_Token := (if Next_Token = TOKEN_LAST then TOKEN_BASE else Next_Token + 1);
      else
         --  No binding, or the publisher failed: stop trying; echo continues.
         Disabled := True;
         for I in 1 .. Used loop
            Lose;
         end loop;
         Used := 0;
      end if;
   end Pump;

   procedure Collect (Completion : CuBit.Messages.CompletionEntry) is
      Handled : Boolean;
   begin
      CuBit.Logging.Complete (Writer, Completion, Handled);
   end Collect;

   procedure Flush is
   begin
      while Used > 0 loop
         if Wanted (Logs.Level (Queue (Head))) then
            Publish_Now (Queue (Head));
         end if;
         Head := (Head + 1) mod CAPACITY;
         Used := Used - 1;
      end loop;
   end Flush;

   function Lost return Unsigned_64 is (Losses + CuBit.Logging.Dropped (Writer));
   function Queued_Count return Natural is (Used);
end CuBit.Log;
