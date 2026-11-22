-- Fixed resident WebView pool. Parked slots are unavailable until the engine
-- has loaded about:blank and acknowledged cleared history. This bounds view
-- containers, not asynchronous pipelines, caches, or total engine memory.
package Servo_Tabs with SPARK_Mode, Pure is
   Capacity : constant := 32;
   subtype Slot is Positive range 1 .. Capacity;
   subtype Selection is Natural range 0 .. Capacity;
   type Phase is (Available, Live, Parking);
   type Slots is array (Slot) of Phase;
   type State is record
      Items : Slots := [1 => Live, others => Available];
      Active : Selection := 1;
   end record;
   function Valid (S : State) return Boolean is
     (if S.Active = 0 then (for all I in Slot => S.Items (I) /= Live)
      else S.Items (S.Active) = Live);
   function Count (S : State) return Natural;
   procedure Open_Tab (S : in out State; Added : out Selection)
     with Pre => Valid (S), Post => Valid (S) and then
       (if Added /= 0 then S.Active = Added and S.Items (Added) = Live);
   procedure Select_Tab (S : in out State; I : Slot)
     with Pre => Valid (S), Post => Valid (S);
   procedure Close_Tab (S : in out State; I : Slot)
     with Pre => Valid (S), Post => Valid (S) and S.Items (I) /= Live;
   procedure Parked (S : in out State; I : Slot)
     with Pre => Valid (S), Post => Valid (S);
   procedure Cycle (S : in out State; Backward : Boolean)
     with Pre => Valid (S), Post => Valid (S);
end Servo_Tabs;
