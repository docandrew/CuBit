with Interfaces; use Interfaces;
with CuBit.Log_Records;
with CuBit.Messages;
package CuBit.Logging is
   type Publisher is record
      Busy : Boolean := False;
      Token : Unsigned_64 := 0;
   end record;
   Emissions : Natural := 0;
   Last_Value : CuBit.Log_Records.Log_Record;
   function Pending (Item : Publisher) return Boolean is (Item.Busy);
   function Dropped (Item : Publisher) return Unsigned_64 is (0);
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
     Token : Unsigned_64; Submitted : out Boolean);
   procedure Complete (Item : in out Publisher; Completion : CuBit.Messages.CompletionEntry;
     Handled : out Boolean);
end CuBit.Logging;
