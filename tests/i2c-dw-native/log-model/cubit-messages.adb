package body CuBit.Messages is
   procedure DebugPrint (Text : String) is
      pragma Unreferenced (Text);
   begin Lines := Lines + 1; end DebugPrint;
end CuBit.Messages;
