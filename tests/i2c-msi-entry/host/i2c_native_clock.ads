with DW_I2C_Transfer;
package I2C_Native_Clock is
   procedure Read (Now : out DW_I2C_Transfer.Tick; Available : out Boolean);
end I2C_Native_Clock;
