pragma Ada_2022;
package body ACPI_Global_Lock_Protocol with SPARK_Mode is
   function Framed (Item : Packet) return Boolean is
     (Item.Length = 4 and then Item.Flags = 0 and then Item.Reserved = 0);
   function Label_For (Kind : Operation; Response : Boolean := False) return Unsigned_32 is
     ((if Response then First_Reply else First_Operation) + Unsigned_32 (Operation'Pos (Kind)));
   function Owned_Label (Label : Unsigned_32) return Boolean is
     (Label in First_Operation .. Label_For (Operation'Last));
   function Decode_Request (Item : Packet) return Request is
      Result : Request;
   begin
      if not Framed (Item) or else not Owned_Label (Item.Label) then return Result; end if;
      Result.Kind := Operation'Val (Item.Label - First_Operation);
      if Result.Kind = Prepare then
         if Item.Data (0) = 0 or else Item.Data (1) in 0 | Unsigned_64'Last
           or else Item.Data (2) not in Unsigned_64 (Attempt_Budget'First) .. Unsigned_64 (Attempt_Budget'Last)
           or else Item.Data (3) /= Version then return (others => <>); end if;
         Result.Nonce := Item.Data (0); Result.Deadline := Item.Data (1);
         Result.Budget := Attempt_Budget (Item.Data (2));
      else
         Result.Token := (Item.Data (0), Item.Data (1));
         if not Valid (Result.Token) or else Item.Data (2) /= Version or else Item.Data (3) /= 0
         then return (others => <>); end if;
      end if;
      Result.Valid := True; return Result;
   end Decode_Request;
   function Prepare_Request (Nonce, Deadline : Unsigned_64; Budget : Attempt_Budget) return Packet is
     ((Label => Label_For (Prepare), Data => [Nonce, Deadline, Unsigned_64 (Budget), Version], others => <>));
   function Ticket_Request (Kind : Operation; Token : Ticket) return Packet is
     ((Label => Label_For (Kind), Data => [Token.Domain, Token.Sequence, Version, 0], others => <>));
   function Valid_Result (Kind : Operation; Result : Reply) return Boolean is
   begin
      if not Result.Valid then return False; end if;
      if Kind = Prepare then
         return Result.Outcome = Not_Ready and then
           (if Result.Kind = Ready then Valid (Result.Token)
            else Result.Kind not in Recorded | Cached | Checked | No_Result and then Result.Token = No_Ticket);
      end if;
      if Result.Token /= No_Ticket or else Result.Kind = Ready then return False; end if;
      case Result.Kind is
         when Recorded => return Kind in Acquire | Release;
         when Cached => return Kind /= Check_Access;
         when Checked => return Kind = Check_Access;
         when others => return Result.Outcome = Not_Ready;
      end case;
   end Valid_Result;
   function Encode_Reply (Kind : Operation; Result : Reply) return Packet is
     ((Label => Label_For (Kind, True),
       Data => [Unsigned_64 (Reply_Kind'Pos (Result.Kind)),
                (if Kind = Prepare then 0 else Unsigned_64 (Lock_Outcome'Pos (Result.Outcome))),
                Result.Token.Domain, Result.Token.Sequence], others => <>));
   function Decode_Reply (Item : Packet; Kind : Operation) return Reply is
      Result : Reply;
   begin
      if not Framed (Item) or else Item.Label /= Label_For (Kind, True)
        or else Item.Data (0) > Reply_Kind'Pos (Reply_Kind'Last) then return Result; end if;
      Result.Kind := Reply_Kind'Val (Item.Data (0));
      if Kind = Prepare then
         if Item.Data (1) /= 0 then return (others => <>); end if;
      else
         if Item.Data (1) > Lock_Outcome'Pos (Lock_Outcome'Last) then return (others => <>); end if;
         Result.Outcome := Lock_Outcome'Val (Item.Data (1));
      end if;
      Result.Token := (Item.Data (2), Item.Data (3)); Result.Valid := True;
      if not Valid_Result (Kind, Result) then return (others => <>); end if;
      return Result;
   end Decode_Reply;
end ACPI_Global_Lock_Protocol;
