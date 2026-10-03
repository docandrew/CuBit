package body Client_Input_Batch_Cache with SPARK_Mode is
   procedure Load
     (S : in out State; Page : W.Snapshot_Words; Receipt : DP.Wire_Message;
      Surface, Request : W.Identity; After : W.Word; Accepted : out Boolean)
   is
      Reply : constant P.Receipt := P.Decode (Receipt, Request);
      Decoded : constant W.Decoding := W.Decode (Page, Surface, Request, After);
   begin
      Accepted := False;
      if Remaining (S) /= 0 or else Reply.Status /= DP.Success or else not Decoded.Accepted then return; end if;
      if Reply.Length /= Decoded.Value.Length or else Reply.Through /= Decoded.Value.Through
        or else Reply.More /= Decoded.Value.More then return; end if;
      S := (Decoded.Value, 0, Surface, After);
      Accepted := True;
   end Load;
   procedure Take
     (S : in out State; Surface : W.Identity; After : W.Word;
      Value : out DP.Input_Result)
   is
   begin
      Value := (Status => DP.Invalid_Request);
      if Remaining (S) = 0 or else S.Surface /= Surface or else S.After /= After then return; end if;
      declare
         Item : constant W.B.IQ.Event := S.Stored.Items (S.Used + 1);
         More : constant Boolean := Remaining (S) > 1 or else S.Stored.More;
      begin
         if Item.Serial <= After or else Item.Kind = 0 then return; end if;
         Value := DP.Decode_Input_Result
           ((DP.Code (DP.Poll_Input), 4, (if More then 1 else 0), 0,
             [Item.Kind, Item.Serial, Item.Payload0, Item.Payload1]), DP.Poll_Input);
         if Value.Status = DP.Success and then Value.Value.Serial = Item.Serial then
            S.Used := S.Used + 1;
            S.After := Item.Serial;
         else
            Value := (Status => DP.Invalid_Request);
         end if;
      end;
   end Take;
   procedure Clear (S : out State) is
   begin S := (others => <>); end Clear;
end Client_Input_Batch_Cache;
