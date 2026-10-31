pragma Ada_2022;
with Intel_GPIO_IRQ;
package GPIO_Native_Clock is
   type Outcome is (Sampled, Unavailable, Reversed);
   procedure Read (Now : out Intel_GPIO_IRQ.Tick; Available : out Boolean);
   function Last_Outcome return Outcome;
   -- Process-lifetime, serialized GETTIME millisecond continuity. Never mix
   -- scheduler deadlines with the ACPI microsecond interface.
end GPIO_Native_Clock;
