pragma Ada_2022;
with CuBit.Handles;
with CuBit.Messages;
with HID_I2C_Transactions;
with I2C_Transfer_Wire;
with CuBit.Grant_References;
with CuBit.Process_IDs;
generic
   Controller : CuBit.Handles.Endpoint;
package HID_I2C_Native_Client is
   type Binding_Status is (Endpoint_Resolved, Endpoint_Refused, Wrong_Endpoint_Kind, Invalid_Endpoint_Identity);
   type Binding (Status : Binding_Status := Endpoint_Refused) is record
      case Status is
         when Endpoint_Resolved => Sender : CuBit.Process_IDs.Process_ID;
         when Endpoint_Refused => Why : CuBit.Handles.Refusal;
         when others => null;
      end case;
   end record;
   function Resolve return Binding;
   -- Inspect this exact endpoint once at admission; retain its opaque process
   -- identity for completion checks. No service-name lookup or peer PID guess.
   -- Endpoint comes from generated Startup, already restricted to one channel.
   procedure Submit (Item : I2C_Transfer_Wire.Request;
                     Token : I2C_Transfer_Wire.Word; Accepted : out Boolean);
   procedure Revoke (Item : CuBit.Grant_References.Reference; Accepted : out Boolean);
   function Retired (Item : CuBit.Grant_References.Reference) return Boolean;
   package Transactions is new HID_I2C_Transactions (Submit, Revoke, Retired);
   procedure Receive
     (S : in out Transactions.State; Entry_Value : CuBit.Messages.CompletionEntry;
      Now : I2C_Transfer_Wire.Word; Clock_Available : Boolean;
      Result : out Transactions.Completion_Result);
   -- Called by the owner's completion dispatcher, never drains other tokens.
   -- Expected sender passed to Transactions.Start comes from a successful
   -- Resolve for this endpoint; never learn it from the first response.
   -- Caller creates grants through this endpoint on its own page allocations.
   -- This adapter does not allocate/free buffers, block or acquire foreign RAM.
end HID_I2C_Native_Client;
