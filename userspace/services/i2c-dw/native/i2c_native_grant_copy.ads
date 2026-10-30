pragma Ada_2022;
with System;
with Interfaces;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with DW_I2C_Transfer;
with I2C_Grant_Copy;
generic
   type Bytes is array (DW_I2C_Transfer.Count range <>) of DW_I2C_Transfer.Byte;
package I2C_Native_Grant_Copy is
   procedure Acquire
     (Reference : CuBit.Grant_References.Reference;
      Owner : CuBit.Process_IDs.Process_ID; Length : Interfaces.Unsigned_64;
      Writable : Boolean; Address : out System.Address; Acquired : out Boolean);
   procedure Release
     (Reference : CuBit.Grant_References.Reference; Returned : out Boolean);
   package Copy is new I2C_Grant_Copy (Bytes, Acquire, Release);
end I2C_Native_Grant_Copy;
