pragma Ada_2022;
with I2C_Transfer_Wire;
package HID_Native_Clock is
   procedure Sample_Time (Now : out I2C_Transfer_Wire.Word; Available : out Boolean);
   procedure Cache_Time (Now : I2C_Transfer_Wire.Word; Available : Boolean);
   procedure Cached_Time (Now : out I2C_Transfer_Wire.Word; Available : out Boolean);
   -- Pump normalizes monotonicity before Cache_Time. Reset.Clock receives only
   -- Cached_Time; all deadlines use GETTIME's epoch, including failure status.
end HID_Native_Clock;
