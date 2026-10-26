with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Log_Records;
package CuBit.Logging is
   type Publisher is record
      Target : CuBit.Handles.Endpoint;
   end record;
   function Awaiting_Grant return Publisher is ((Target => CuBit.Handles.No_Endpoint));
   procedure Attach (Item : in out Publisher; Target : CuBit.Handles.Endpoint);
   Attachments : Natural := 0;
   Emissions : Natural := 0;
   Last_Value : CuBit.Log_Records.Log_Record;
   function Dropped (Item : Publisher) return Unsigned_64 is (0);
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
     Submitted : out Boolean);
end CuBit.Logging;
