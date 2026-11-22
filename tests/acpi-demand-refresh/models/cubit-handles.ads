package CuBit.Handles with Pure is
 type Reply is private;
 No_Reply:constant Reply;
 function Valid(Item:Reply) return Boolean;
private
 Maximum_Slots:constant:=16;
 type Reply is record Slot:Natural range 0..Maximum_Slots:=0;end record;
 No_Reply:constant Reply:=(Slot=>0);
end CuBit.Handles;
