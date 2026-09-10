package body CuBit.Desktop_Messages is
   function To_Wire (Item : CuBit.Messages.Message) return
     CuBit.Desktop_Protocol.Wire_Message is
     (Item.tag.label, Item.tag.length, Item.tag.flags, Item.tag.reserved,
      CuBit.Desktop_Protocol.Payload (Item.words));
   function From_Wire (Item : CuBit.Desktop_Protocol.Wire_Message) return
     CuBit.Messages.Message is
     (tag => (Item.Label, Item.Length, Item.Flags, Item.Reserved),
      authorityTag => 0, words => CuBit.Messages.MessageWords (Item.Words));
end CuBit.Desktop_Messages;
