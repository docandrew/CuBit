pragma Ada_2022;
with Interfaces;
--  Startup decisions only. Caller supplies authenticated admission/inspection
--  results, owns child creation/stop, and retains uncertain broker resources.
package CuBit.Render_Startup with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64;
   type Requirement is (Required, Optional);
   type Attempt is (With_Render, Software_Only);
   type Admission is (Not_Requested, Pending, Rejected, Admitted, Uncertain);
   type Capability is (Unknown, Empty, Render_Endpoint, Other);
   type Decision is
     (Resume_Render, Resume_Software, Discard, Discard_Then_Software);
   function Initial_Attempt
     (Demand : Requirement; Approved : Boolean) return Attempt is
     (if Demand = Optional and not Approved then Software_Only
      else With_Render);
   function Decide
     (Demand : Requirement; Mode : Attempt; Approved : Boolean;
      Result : Admission; Slot : Capability) return Decision is
     (if Mode = Software_Only then
         (if Demand = Optional and Result = Not_Requested and Slot = Empty
          then Resume_Software else Discard)
      elsif Approved and Result = Admitted and Slot = Render_Endpoint then
         Resume_Render
      elsif Demand = Optional then Discard_Then_Software
      else Discard)
     with Post =>
       (Decide'Result = Resume_Render) =
         (Mode = With_Render and Approved and Result = Admitted and
          Slot = Render_Endpoint) and
       (Decide'Result = Resume_Software) =
         (Mode = Software_Only and Demand = Optional and
          Result = Not_Requested and Slot = Empty) and
       (if Demand = Required then Decide'Result in Resume_Render | Discard) and
       (if Result in Pending | Rejected | Uncertain then
          Decide'Result not in Resume_Render | Resume_Software) and
       (if Mode = Software_Only then Decide'Result /= Discard_Then_Software);
   function Fresh_Retry
     (Prior, Current : Interfaces.Unsigned_64) return Boolean is
     (Prior mod 2 ** 32 /= 0 and Prior / 2 ** 32 /= 0 and
      Current mod 2 ** 32 /= 0 and Current / 2 ** 32 /= 0 and
      Prior /= Current)
     with Post => (if Fresh_Retry'Result then
       Prior /= 0 and Current /= 0 and Prior /= Current);
   --  Stop acceptance is not GPU retirement. A retry creates a fresh child;
   --  never recycle the failed attempt's identity, source slots or tokens.
   --  The sole retry is Software_Only; Decide cannot request a third launch.
   function Retry_Software
     (Next : Decision; Stop_Accepted : Boolean) return Boolean is
     (Next = Discard_Then_Software and Stop_Accepted)
     with Post => (if Retry_Software'Result then
       Stop_Accepted and Next = Discard_Then_Software);
end CuBit.Render_Startup;
