pragma Ada_2022;
with CuBit.Handles;
with CuBit.Messages;
with HID_I2C_Grant_Creation;
with HID_I2C_Descriptors;
with HID_I2C_Native_Client;
with HID_I2C_Buffered_Client;
with I2C_Transfer_Wire;
with CuBit.Grant_References;
generic
   Maximum_Transfer : I2C_Transfer_Wire.Capacity;
   Controller : CuBit.Handles.Endpoint;
package HID_I2C_Native_Buffers is
   package W renames I2C_Transfer_Wire;
   package D renames HID_I2C_Descriptors;
   package Endpoint_Client is new HID_I2C_Native_Client (Controller);
   procedure Allocate (Bytes : W.Word; Address : out W.Word; OK : out Boolean);
   procedure Release (Address, Bytes : W.Word; OK : out Boolean);
   procedure Clear (Address, Bytes : W.Word);
   procedure Store (Address : W.Word; Data : D.Bytes);
   procedure Load (Address : W.Word; Data : out D.Bytes);
   procedure Create (Address, Pages : W.Word; Writable : Boolean;
                     Reference : out CuBit.Grant_References.Reference; Status : out HID_I2C_Grant_Creation.Status);
   package Client is new HID_I2C_Buffered_Client
     (Maximum_Transfer, Allocate, Release, Clear, Store, Load, Create,
      Endpoint_Client.Submit, Endpoint_Client.Revoke, Endpoint_Client.Retired);
   procedure Receive (S : in out Client.State; Entry_Value : CuBit.Messages.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean; Result : out Client.Transactions.Completion_Result);
   -- Callbacks are trusted internal adapters, only for Client's owned spans.
   -- No client-supplied pointer is ever admitted and no physical/MMIO mapping
   -- occurs here. Allocate/Release use exact own-allocation syscalls. Whole
   -- grant pages contain only buffer bytes; Client owns no adjacent metadata.
end HID_I2C_Native_Buffers;
