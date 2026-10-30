pragma Ada_2022;
with Interfaces;
with System;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with DW_I2C_Transfer;
with DW_I2C_Request_Types;
generic
   type Bytes is array (DW_I2C_Transfer.Count range <>) of DW_I2C_Transfer.Byte;
   with procedure Acquire
     (Reference : CuBit.Grant_References.Reference;
      Owner : CuBit.Process_IDs.Process_ID; Length : Interfaces.Unsigned_64;
      Writable : Boolean; Address : out System.Address; Acquired : out Boolean);
   with procedure Release
     (Reference : CuBit.Grant_References.Reference; Returned : out Boolean);
package I2C_Grant_Copy with SPARK_Mode => Off is
   procedure Read_Input
     (Caller : CuBit.Process_IDs.Process_ID; Grant : CuBit.Grant_References.Reference;
      Data : out Bytes; Result : out DW_I2C_Request_Types.Copy_Result);
   procedure Write_Output
     (Caller : CuBit.Process_IDs.Process_ID; Grant : CuBit.Grant_References.Reference;
      Data : Bytes; Result : out DW_I2C_Request_Types.Copy_Result);
   -- Native Acquire must authenticate this exact expected owner/reference,
   -- return a mapping covering Length bytes at offset zero, and pin its lifetime
   -- until Release. No address is derived from grant identity. Each successful
   -- acquisition is returned once; a failed return is Uncertain, never retried.
   -- Serialized callbacks must not raise. Volatile overlays are unproved and
   -- trust the kernel's mapping validity, permissions and lifetime guarantees.
end I2C_Grant_Copy;
