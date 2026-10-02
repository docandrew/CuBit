package body CuBit.Logging is
   procedure Emit (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
     Token : Unsigned_64; Submitted : out Boolean) is
   begin
      pragma Assert (not Item.Busy);
      Item.Busy := True; Item.Token := Token;
      Last_Value := Value; Emissions := Emissions + 1; Submitted := True;
   end;
   procedure Complete (Item : in out Publisher; Completion : CuBit.Messages.CompletionEntry;
     Handled : out Boolean) is
   begin
      Handled := Item.Busy and then Item.Token = Completion.token;
      if Handled then Item.Busy := False; end if;
   end;
end CuBit.Logging;
