pragma Ada_2022;
with DW_I2C_Transfer;
package I2C_Native_Clock is
   type Outcome is (Sampled, Unavailable, Reversed);
   procedure Read (Now : out DW_I2C_Transfer.Tick; Available : out Boolean);
   function Last_Outcome return Outcome;
   -- One serialized process-lifetime continuity check across ALL owner/port/
   -- reset/config callback samples, in GETTIME scheduler milliseconds. Never
   -- mix this clock with ACPI's separate microsecond interface.
end I2C_Native_Clock;
