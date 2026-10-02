with Interfaces;
with CuBit.Metric_Records;
with Compositor_Elapsed;
-- Tracks successful appends to the SDK's current filling page. No second
-- record queue or page storage: the SDK still owns its two bounded pages.
package Compositor_Metric_Batch_Policy with SPARK_Mode, Pure is
   subtype Tick is Interfaces.Unsigned_64;
   use type Tick;
   subtype Count is CuBit.Metric_Records.Record_Count;
   Capacity : constant := CuBit.Metric_Records.Maximum_Records;
   Flush_Interval_Us : constant Tick := 100_000;
   Declaration_Count : constant := 6;
   type Append_Kind is (Describe_Output_0, Describe_Output_1,
                       Describe_Input, Describe_Request, Describe_Draw, Describe_Submit,
                       Measurement, Full);
   subtype Description is Append_Kind range Describe_Output_0 .. Describe_Submit;
   type State is private;
   function Used (S : State) return Count;
   function First (S : State) return Tick;
   function Next (S : State) return Append_Kind;
   function Samples (S : State) return Count is
     (if Used (S) > Declaration_Count then Used (S) - Declaration_Count else 0);
   function Due (S : State; Now : Tick) return Boolean;
   function Delay_Us (S : State; Now : Tick) return Tick
     with Post => Delay_Us'Result = Compositor_Elapsed.Unavailable or else
       Delay_Us'Result <= Flush_Interval_Us;
   function Wake_At_Ms (Now_Ms, Pause_Us : Tick) return Tick
     with Pre => Now_Ms /= Compositor_Elapsed.Unavailable and Pause_Us <= Flush_Interval_Us,
       Post => Wake_At_Ms'Result >= Now_Ms and Wake_At_Ms'Result < Tick'Last;
   -- Call only after the actual SDK accepted the selected record. Refusal
   -- leaves the policy untouched; a later attempt repeats missing metadata.
   procedure Accepted (S : in out State; Now : Tick)
     with Pre => Next (S) /= Full and Now /= Compositor_Elapsed.Unavailable,
       Post => Used (S) = Used (S'Old) + 1 and
         First (S) = (if Used (S'Old) = 0 then Now else First (S'Old));
   -- Call only after a successful SDK seal/submission has moved to its next
   -- filling page. A failed/uncertain submission must not reopen this page.
   procedure Submitted (S : out State)
     with Post => Used (S) = 0 and First (S) = Compositor_Elapsed.Unavailable;
private
   type State is record
      Records : Count := 0;
      Started : Tick := Compositor_Elapsed.Unavailable;
   end record with Dynamic_Predicate =>
     ((State.Records = 0) = (State.Started = Compositor_Elapsed.Unavailable));
   function Used (S : State) return Count is (S.Records);
   function First (S : State) return Tick is (S.Started);
   function Next (S : State) return Append_Kind is
     (case S.Records is
         when 0 => Describe_Output_0,
         when 1 => Describe_Output_1,
         when 2 => Describe_Input,
         when 3 => Describe_Request,
         when 4 => Describe_Draw,
         when 5 => Describe_Submit,
         when Declaration_Count .. Capacity - 1 => Measurement,
         when Capacity => Full);
   function Due (S : State; Now : Tick) return Boolean is
     (Samples (S) > 0 and then
       (Used (S) = Capacity or else
        not Compositor_Elapsed.Measure (First (S), Now).Valid or else
        Compositor_Elapsed.Measure (First (S), Now).Microseconds >= Flush_Interval_Us));
   function Delay_Us (S : State; Now : Tick) return Tick is
     (if Samples (S) = 0 then Compositor_Elapsed.Unavailable
      elsif Due (S, Now) then 0
      else Flush_Interval_Us - Compositor_Elapsed.Measure (First (S), Now).Microseconds);
   function Wake_At_Ms (Now_Ms, Pause_Us : Tick) return Tick is
     (if (Pause_Us + 999) / 1000 > Tick'Last - 1 - Now_Ms then Tick'Last - 1
      else Now_Ms + (Pause_Us + 999) / 1000);
end Compositor_Metric_Batch_Policy;
