with Interfaces;
with CuBit.Messages;
package Desktop_Logs is
   procedure Write (Text : String);
   procedure Pump (Sequence : in out Interfaces.Unsigned_64);
   function Matches (Token : Interfaces.Unsigned_64) return Boolean;
   procedure Collect (Completion : CuBit.Messages.CompletionEntry);
end Desktop_Logs;
