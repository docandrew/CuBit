with CuBit.Messages;
with CuBit.Clocks;
with CuBit.Clock_Control;
package Wall_Clock is
   procedure Initialize;
   --  Publish the kernel's wall-clock offset (SYSINFO_WALL_CLOCK_OFFSET) so
   --  every process reads UTC without asking this service. Only the
   --  registered clock service may; call after registering, after every
   --  adjustment, and as the discipline advances.
   procedure Publish;
   function Snapshot return CuBit.Messages.Message;
   --  Caller has already checked clock-control authority.
   procedure Adjust
     (Candidate : CuBit.Clock_Control.Sample;
      Result : out CuBit.Clock_Control.Outcome;
      Quality : out CuBit.Clocks.Time_Quality);
end Wall_Clock;
