with Interfaces;
with CuBit.Handles;
package Boot_RTC is
   --  Boot-only CMOS access under devmgr's I/O authority (Ports must cover
   --  the CMOS index and data ports), before userspace applications start.
   --  Never writes clock/date registers.
   procedure Read (Ports : CuBit.Handles.IO_Ports;
                   UTC_Seconds, Monotonic_Ms : out Interfaces.Unsigned_64;
                   Valid : out Boolean);
end Boot_RTC;
