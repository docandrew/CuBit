pragma Ada_2022;
with Hardware_Authority;
package body ACPI_Region_IO with SPARK_Mode is
   procedure Execute
     (Region : in out State; Hardware : in out Hardware_State;
      Stamp : Unsigned_64; Item : Request; Reply : out Response) is
      Result : Decision;
      Ticket, Value : Unsigned_64;
      Completed, Released : Boolean;
   begin
      Reply := (others => <>);
      if (not Item.For_Write and then Item.Value /= 0) or else
        not Fits_Value (Item.Value, Item.Width)
      then Reply.Status := Malformed; return; end if;
      Begin_Access (Region, Stamp, Item.Token, Item.Offset,
                    Item.Width, Item.For_Write, Result, Ticket);
      if not Result.Allowed then return; end if;
      Transact (Hardware, Policy (Region).Space, Result.Address,
                Item.Width, Item.For_Write, Item.Value, Value, Completed);
      if not Completed then
         Revoke (Region);
         Reply.Status := Indeterminate;
         Reply.Pending_Ticket := Ticket;
         return;
      end if;
      Finish_Access (Region, Ticket, Released);
      pragma Assert (Released);
      if not Item.For_Write and then not Fits_Value (Value, Item.Width) then
         Revoke (Region);
         Reply.Status := Backend_Fault;
         return;
      end if;
      Reply.Status := Done;
      if not Item.For_Write then Reply.Value := Value; end if;
   end Execute;
   procedure Dispatch
     (Region : in out State; Hardware : in out Hardware_State;
      Stamp : Unsigned_64; Item : ACPI_Region_Protocol.Packet;
      Reply : out ACPI_Region_Protocol.Packet; Pending_Ticket : out Unsigned_64) is
      Decoded : Request;
      Result : Response;
   begin
      Pending_Ticket := 0;
      Reply := (Label => Operation_Reply,
                Data => [Unsigned_64 (Outcome'Pos (Denied)), 0, 0, 0], others => <>);
      if Stamp = 0 or else Stamp /= Policy (Region).Tag then return; end if;
      Reply.Data (0) := Unsigned_64 (Outcome'Pos (Malformed));
      if Item.Length /= 4 or else Item.Flags /= 0 or else Item.Reserved /= 0
        or else Item.Label not in Read_Operation | Write_Operation
      then return; end if;
      if Item.Data (3) /= 0 or else
        (Item.Label = Read_Operation and then Item.Data (2) /= 0)
      then return; end if;
      Reply.Data (0) := Unsigned_64 (Outcome'Pos (Denied));
      if not Hardware_Authority.Permits
        (Policy (Region).Register_Authority, Item.Data (1),
         Item.Label = Write_Operation, Item.Data (2))
      then return; end if;
      Decoded.Token := Item.Data (0);
      Decoded.Offset := Policy (Region).Register_Offset;
      Decoded.Width := Policy (Region).Register_Width;
      Decoded.For_Write := Item.Label = Write_Operation;
      Decoded.Value := Item.Data (2);
      Execute (Region, Hardware, Stamp, Decoded, Result);
      Reply.Data := [Unsigned_64 (Outcome'Pos (Result.Status)),
                     Result.Value and 16#FFFF_FFFF#, Shift_Right (Result.Value, 32), 0];
      Pending_Ticket := Result.Pending_Ticket;
   end Dispatch;
end ACPI_Region_IO;
