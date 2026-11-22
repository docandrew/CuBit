with CuBit.Log_Records;
package CuBit.Logging is
   type Publisher is limited null record;
   function Bound_To (Target : Natural) return Publisher;
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record; Submitted : out Boolean);
   procedure Disconnect (Item : in out Publisher; Done : out Boolean);
   Emissions, Disconnects, Late_Emissions : Natural := 0;
   Complete : Boolean := True;
   On_Disconnect : access procedure := null;
end CuBit.Logging;
