with Interfaces; use Interfaces;
package Update_Test_Backend is
   Owner : Boolean := True;
   Lose_On_Reserve, Lose_On_Commit, Fail_Commit, Fail_Clear : Boolean := False;
   Reservations, Commits, Clears : Natural := 0;
   Committed : Unsigned_64 := 0;
   function Ready return Boolean;
   function Reserve (Bytes : Unsigned_64) return Unsigned_64;
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean;
   function Clear (Base, Bytes : Unsigned_64) return Boolean;
   procedure Reset;
end Update_Test_Backend;
