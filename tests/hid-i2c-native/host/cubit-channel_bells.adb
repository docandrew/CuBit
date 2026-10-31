package body CuBit.Channel_Bells is
   procedure Note_Notice (Label : Interfaces.Unsigned_32;
      Length : Interfaces.Unsigned_8; W : Words; Is_Notice : out Boolean) is
      pragma Unreferenced (Label, Length, W);
   begin Calls := Calls + 1; Is_Notice := True; end Note_Notice;
end CuBit.Channel_Bells;
