with Interfaces;
-- Validates a native microsecond clock sample for AML Timer. This package
-- does not read hardware or establish the physical clock's accuracy.
package AML_Clock with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype Tick is Interfaces.Unsigned_64;
   Max_Microseconds : constant Tick := Tick'Last / 10;
   procedure No_Sample (Value : out Tick; Available : out Boolean)
     with Global => null, Post => Value = 0 and not Available;
   type State is private;
   function Last_Accepted (Clock : State) return Tick;
   function Fresh return State with Post => Last_Accepted (Fresh'Result) = 0;
   type Sample_Status is (Accepted, Unavailable, Out_Of_Range, Regressed);
   procedure Observe
     (Clock : in out State; Microseconds : Tick; Available : Boolean;
      Value : out Tick; Status : out Sample_Status)
     with Global => null,
       Post =>
         (if Status = Accepted then
            Available and then Microseconds <= Max_Microseconds
            and then Value = Microseconds * 10
            and then Last_Accepted (Clock) = Value
            and then Value >= Last_Accepted (Clock'Old)
          else Value = 0 and then Clock = Clock'Old),
       Contract_Cases =>
         (not Available => Status = Unavailable,
          Available and then Microseconds > Max_Microseconds => Status = Out_Of_Range,
          Available and then Microseconds <= Max_Microseconds
            and then Microseconds * 10 < Last_Accepted (Clock) => Status = Regressed,
          others => Status = Accepted);
private
   type State is record
      Last : Tick := 0;
   end record;
end AML_Clock;
