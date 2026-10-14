with Interfaces;
with CuBit.Messages;
--  Hosted fixture of the offer half of CuBit.Handles.IPC (CAP-002): the
--  ACPI service takes its provider's endpoint offered with the launcher's
--  configuration call. Offer_Present says whether a request carries an
--  offer; an accepted one is the endpoint of slot Offer_Slot (generation 1).
package CuBit.Handles.IPC is
   subtype Message is CuBit.Messages.Message;
   type Carried_Kind is (Absent, Present, Refused);
   type Carried (Kind : Carried_Kind := Absent) is record
      case Kind is
         when Absent  => null;
         when Present => Word : Interfaces.Unsigned_64 := 0;
         when Refused => Why  : Refusal := ABI.Limit_Reached;
      end case;
   end record;
   type Accepted_Endpoint (OK : Boolean := False) is record
      case OK is
         when True  => Value : Endpoint;
         when False => Why   : Refusal := ABI.Wrong_Kind;
      end case;
   end record;
   Offer_Present : Boolean := True;
   Offer_Slot : Interfaces.Unsigned_64 := 7;
   Accepted_Offers : Natural := 0;
   function Offers (Request : Message) return Boolean;
   function Accept_Offered (Requester : CuBit.Messages.Process_ID) return Carried;
   function Accept_Endpoint (Item : Carried) return Accepted_Endpoint;
end CuBit.Handles.IPC;
