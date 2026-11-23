package body CuBit.Handles is
 function Valid(Item:Reply) return Boolean is (Item.Slot/=0);
end CuBit.Handles;
