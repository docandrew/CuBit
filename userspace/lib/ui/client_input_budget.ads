with Interfaces;
-- Bounds work admitted between rendering opportunities. Time is milliseconds;
-- an individual event handler remains responsible for its own execution bound.
package Client_Input_Budget with SPARK_Mode, Pure is
   subtype Tick is Interfaces.Unsigned_64;
   use type Tick;
   Poll_Limit : constant := 32;
   Time_Limit : constant Tick := 1;
   subtype Poll_Count is Natural range 0 .. Poll_Limit;
   type Batch is private;
   function Used (S : Batch) return Poll_Count;
   function Started (S : Batch) return Tick;
   function Open (Now : Tick) return Batch
     with Post => Used (Open'Result) = 0 and Started (Open'Result) = Now;
   function Can_Poll (S : Batch; Now : Tick) return Boolean
     with Post => Can_Poll'Result =
       (Used (S) < Poll_Limit and then
        (Used (S) = 0 or else
         (Now >= Started (S) and then Now - Started (S) < Time_Limit)));
   procedure Charge (S : in out Batch)
     with Pre => Used (S) < Poll_Limit,
       Post => Used (S) = Used (S)'Old + 1 and Started (S) = Started (S)'Old;
private
   type Batch is record
      Count : Poll_Count := 0;
      Start : Tick := 0;
   end record;
   function Used (S : Batch) return Poll_Count is (S.Count);
   function Started (S : Batch) return Tick is (S.Start);
end Client_Input_Budget;
