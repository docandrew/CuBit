with Interfaces;
package Boot_RTC is
   --  Boot-only CMOS access under devmgr's existing I/O authority, before
   --  userspace applications start. Never writes clock/date registers.
   procedure Read (UTC_Seconds, Monotonic_Ms : out Interfaces.Unsigned_64;
                   Valid : out Boolean);
end Boot_RTC;
