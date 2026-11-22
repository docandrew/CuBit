pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
with Interfaces;
package HID_Native_Queues is
   procedure Poll_Completion (Item : out CuBit.Messages.CompletionEntry;
      Found : out Boolean);
   procedure Poll_Event (Publisher : out CuBit.Process_IDs.Process_ID;
      Item : out CuBit.Messages.Message; Found : out Boolean);
   procedure Drain_Request (Found : out Boolean);
   function Last_Reply_Status return Interfaces.Unsigned_64;
   -- No request API is exported by this publisher. Answer unexpected requests
   -- with the existing empty-reply convention. Offers are never accepted and
   -- end with the call; no carried capability or saved reply is retained.
end HID_Native_Queues;
