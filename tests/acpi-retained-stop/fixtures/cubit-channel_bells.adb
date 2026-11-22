package body CuBit.Channel_Bells is
 procedure Note_Notice(Label:Interfaces.Unsigned_32;Length:Interfaces.Unsigned_8;Data:Words;Noted:out Boolean) is
 pragma Unreferenced(Label,Length,Data);begin Calls:=Calls+1;Noted:=True;end;
end CuBit.Channel_Bells;