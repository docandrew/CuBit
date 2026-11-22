with Interfaces; use Interfaces;

--  Portable, time-based recognition. Caller supplies physical edges, stable
--  target identity and monotonic milliseconds; rendering never advances time.
package CuBit.Click_Sequences with SPARK_Mode, Pure is
   type Target_ID is new Unsigned_64;
   No_Target : constant Target_ID := 0;
   type Point is record
      X, Y : Natural := 0;
   end record;
   type Policy is record
      Interval_Ms : Unsigned_64 := 500;
      Slop : Natural := 4;
   end record;
   Default_Policy : constant Policy := (others => <>);
   type Press_Kind is (Single_Press, Double_Press);
   type State is private;
   function Needs_Release (Item : State) return Boolean;
   procedure Reset (Item : out State);
   procedure Motion (Item : in out State; At_Point : Point;
                     Settings : Policy := Default_Policy);
   procedure Press
     (Item : in out State; Target : Target_ID; At_Point : Point;
      Now_Ms : Unsigned_64; Kind : out Press_Kind;
      Settings : Policy := Default_Policy);
   procedure Release
     (Item : in out State; At_Point : Point; Now_Ms : Unsigned_64;
      Settings : Policy := Default_Policy);
private
   type Phase is (Idle, Pressed, Released);
   type State is record
      Stage : Phase := Idle;
      Target : Target_ID := No_Target;
      Origin : Point;
      Press_Ms : Unsigned_64 := 0;
      Last_Ms : Unsigned_64 := 0;
   end record;
   function Needs_Release (Item : State) return Boolean is
     (Item.Stage = Pressed);
end CuBit.Click_Sequences;
