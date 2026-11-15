with Fixture;
package body I2C_Native_Clock is
   procedure Read (Now : out DW_I2C_Transfer.Tick; Available : out Boolean) is
   begin Now := DW_I2C_Transfer.Tick (Fixture.Now); Available := Fixture.Clock_OK; end Read;
end I2C_Native_Clock;
