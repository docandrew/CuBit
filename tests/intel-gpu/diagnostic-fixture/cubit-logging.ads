with Interfaces; use Interfaces;
with CuBit.Log_Records;
package CuBit.Logging is
   type Publisher is record
      Unused : Boolean := False;
   end record;
   Emissions : Natural := 0;
   Last_Value : CuBit.Log_Records.Log_Record;
   function Dropped (Item : Publisher) return Unsigned_64 is (0);
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
     Submitted : out Boolean);
end CuBit.Logging;
