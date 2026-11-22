package body Proof_Device is
   procedure Transfer
     (Item : Transfer_Request; Buffer : System.Address; Moved : out Unsigned_64)
   is
      pragma Unreferenced (Buffer);
   begin
      pragma Assert (Valid_Transfer (Current, Item));
      Last_Item := Item;
      if Transfers < Natural'Last then
         Transfers := Transfers + 1;
      end if;
      Moved := (if Short_By <= Bytes (Item) then Bytes (Item) - Short_By else 0);
   end Transfer;

   procedure Flush (Durable : out Boolean) is
   begin
      pragma Assert (Flushes (Current));
      if Flush_Calls < Natural'Last then
         Flush_Calls := Flush_Calls + 1;
      end if;
      Durable := Flush_Result;
   end Flush;
end Proof_Device;
