with CuBit.Messages;
with CuBit.Clocks;
with CuBit.Clock_Control;
package Wall_Clock is
   procedure Initialize;
   function Snapshot return CuBit.Messages.Message;
   --  Caller has already checked clock-control authority.
   procedure Adjust
     (Candidate : CuBit.Clock_Control.Sample;
      Result : out CuBit.Clock_Control.Outcome;
      Quality : out CuBit.Clocks.Time_Quality);
end Wall_Clock;
