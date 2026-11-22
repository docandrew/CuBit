pragma Ada_2022;
with Interfaces;
package body ACPI_Firmware_Data_Dispatch is
   use type P.Word, Interfaces.Unsigned_32;
   procedure Count (Value : in out P.Word) is
   begin if Value < P.Word'Last then Value := Value + 1; end if; end Count;
   function Observe (Owner : Context) return Statistics is
      ((Owner.Accepted,Owner.Malformed,Owner.Denied,Owner.Reentered,Owner.Working));
   procedure Dispatch (Owner : in out Context; Caller : P.Word;
      Request : IPC.Packet; Has_Capability : Boolean;
      Response : out IPC.Packet; Handled : out Boolean) is
      Parsed : constant F.Decoded := F.Decode (Request,Has_Capability);
      Reply : P.Reply;
   begin
      Response := (Length => 0,others => <>);
      Handled := Request.Label >= F.First_Label and then Request.Label <= F.Label_For (F.Operation'Last);
      if not Handled then return; end if;
      Response.Label := Request.Label + F.Reply_Delta;
      if not Parsed.Valid then Count (Owner.Malformed); return; end if;
      declare Item : constant F.Request := Parsed.Item; begin
         Reply.Kind := Item.Kind; Reply.Correlation := Item.ID;
         case Item.Kind is
            when F.Select_Datum => Reply.Auxiliary := Item.Client_Descriptor_ID;
            when F.Read_Datum | F.Query_Operation => Reply.Auxiliary := Item.Operation_ID;
            when F.Write_Datum => Reply.Auxiliary := Item.Write_Operation_ID;
            when others => null;
         end case;
         if Owner.Working then
            Count (Owner.Reentered); Reply.Status := P.Busy;
            Response := P.Encode (Reply); return;
         end if;
         Owner.Working := True;
         if Caller = 0 or else not Authorized (Caller) then
            Count (Owner.Denied); Reply.Status := P.Denied;
         else
            Count (Owner.Accepted);
            Handle_Request (Caller,Item,Reply.Status,Reply.Effect,Reply.Value);
         end if;
         -- Never forward an inconsistent callback result as usable success.
         if P.Valid (Reply) then Response := P.Encode (Reply); end if;
         Owner.Working := False;
      end;
   end Dispatch;
end ACPI_Firmware_Data_Dispatch;
