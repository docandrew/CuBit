package body CuBit.Handles.IPC is
   use type Interfaces.Unsigned_64;
   Generation_One : constant Interfaces.Unsigned_64 := 2 ** 32;
   function Offers (Request : Message) return Boolean is
     (Offer_Present);
   function Accept_Offered (Requester : CuBit.Messages.Process_ID) return Carried is
   begin
      if not Offer_Present then
         return (Kind => Refused, Why => ABI.Not_Accepted);
      end if;
      Accepted_Offers := Accepted_Offers + 1;
      return (Kind => Present, Word => Offer_Slot + Generation_One);
   end Accept_Offered;
   function Accept_Endpoint (Item : Carried) return Accepted_Endpoint is
     (if Item.Kind = Present then (OK => True, Value => (Word => Item.Word))
      else (OK => False, Why => ABI.Not_Accepted));
end CuBit.Handles.IPC;
