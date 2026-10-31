pragma Ada_2022;
with Interfaces;
package Delay_Model with Preelaborate is
   Now : Interfaces.Unsigned_64 := 0;
   Step : Interfaces.Unsigned_64 := 1;
   Reads, Yields, Sleeps : Natural := 0;
   Available : Boolean := True;
   Regress, Stopped, Late, Refuse_Yield, Refuse_Sleep : Boolean := False;
   procedure Read (Value : out Interfaces.Unsigned_64; Ready : out Boolean);
   function Yield_Result return Interfaces.Unsigned_64;
   function Sleep_Result (Target : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
end Delay_Model;
