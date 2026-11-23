with Interfaces;
with CuBit.Handles;
package Boot_RTC is
   --  Boot-only CMOS access under devmgr's I/O authority (Ports must cover
   --  the CMOS index and data ports), before userspace applications start.
   --  Never writes clock/date registers.
   type Handover_History is record
      Selector_Completed : Boolean := False;
      NMI_Disabled : Boolean := False;
      -- A stable MC146818-style register snapshot identifies the baseline
      -- legacy protocol even when its date is invalid or the clock is unset.
      Legacy_Validated : Boolean := False;
   end record;
   procedure Read (Ports : CuBit.Handles.IO_Ports;
                   UTC_Seconds, Monotonic_Ms : out Interfaces.Unsigned_64;
                   Valid : out Boolean; History : out Handover_History);
end Boot_RTC;
