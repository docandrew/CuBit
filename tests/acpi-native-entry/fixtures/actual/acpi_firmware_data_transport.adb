pragma Ada_2022;
package body ACPI_Firmware_Data_Transport with SPARK_Mode is
   use type F.Operation, IPC.Time_Instant, IPC.Delivery, P.Effect_Kind, P.Outcome, P.Word;
   function Observe (Owner : Context) return Statistics is
      ((Owner.Working,Owner.Cleanup_Due,Owner.Clock_Broken));
   procedure Cleanup (Owner : in out Context; Closed : out Boolean) is
      Copy : Carried := Owner.Held;
   begin
      Closed := True;
      if not Owner.Cleanup_Due then return; end if;
      Dispose (Copy,Closed);
      if Closed then Owner.Held := No_Carried; Owner.Cleanup_Due := False; end if;
   end Cleanup;
   procedure Recover_Cleanup (Owner : in out Context; Closed : out Boolean) is
   begin
      Closed := False; if Owner.Working then return; end if;
      Owner.Working := True; Cleanup (Owner,Closed); Owner.Working := False;
   end Recover_Cleanup;
   procedure Check_Time (Owner : in out Context; Deadline : IPC.Time_Instant;
      Status : out Outcome) is
      Now : IPC.Time_Instant;
      Available : Boolean;
   begin
      Status := Clock_Reversed;
      if Owner.Clock_Broken then return; end if;
      Clock (Now,Available);
      if not Available then Status := Clock_Unavailable; return;
      elsif Owner.Seen_Clock and then Now < Owner.Last_Clock then
         Owner.Clock_Broken := True; return;
      end if;
      Owner.Seen_Clock := True; Owner.Last_Clock := Now;
      Status := (if Now >= Deadline then Expired else Completed);
   end Check_Time;
   procedure Call (Owner : in out Context; Connection : in out Channel;
      Item : in out Attempt; Request : F.Request; Expected : P.Effect_Kind;
      Width : F.T.Access_Width; Deadline : IPC.Time_Instant; Reply : out Result) is
      Response : IPC.Packet;
      Received : Carried := No_Carried;
      Delivered : IPC.Delivery;
      Decoded : P.Decoded;
      Closed : Boolean;
   begin
      Reply := (others => <>);
      if Owner.Working then Reply.Status := Reentered; return;
      elsif Item.Used then Reply.Status := Already_Attempted; return; end if;
      Item.Used := True;
      if not F.Valid (Request)
        or else (Request.Kind in F.Read_Datum | F.Query_Operation and then Expected = P.No_Effect)
        or else (Request.Kind = F.Read_Datum and then Expected /= P.Read_Effect)
        or else (Request.Kind = F.Write_Datum and then Expected /= P.Write_Effect)
        or else (Request.Kind not in F.Read_Datum | F.Write_Datum | F.Query_Operation and then Expected /= P.No_Effect)
      then return;
      elsif Deadline = 0 or else Deadline = IPC.No_Finite_Deadline then Reply.Status := Invalid_Deadline; return;
      elsif Owner.Cleanup_Due then Reply.Status := Cleanup_Pending; return; end if;
      Owner.Working := True;
      Check_Time (Owner,Deadline,Reply.Status);
      if Reply.Status /= Completed then Owner.Working := False; return; end if;
      Reply.Status := Completion_Unknown;
      Exchange (Connection,Deadline,F.Encode (Request),Response,Received,Delivered);
      if Present (Received) then
         Owner.Held := Received; Owner.Cleanup_Due := True;
         Cleanup (Owner,Closed);
         Reply.Status := (if Closed then Malformed_Reply else Cleanup_Pending);
      elsif Delivered = IPC.Not_Dispatched then Reply.Status := Not_Sent;
      elsif Delivered = IPC.Reply_Received then
         Decoded := P.Decode (Response,Request,Expected,False);
         if not Decoded.Valid then Reply.Status := Malformed_Reply;
         elsif Expected = P.Read_Effect and then Decoded.Item.Status = P.Done
           and then Decoded.Item.Value > F.L.Mask (Width) then Reply.Status := Malformed_Reply;
         else
            Check_Time (Owner,Deadline,Reply.Status);
            if Reply.Status = Completed then Reply.Reply := Decoded;
            elsif Reply.Status = Expired then Reply.Status := Late_Reply; end if;
         end if;
      end if;
      Owner.Working := False;
   end Call;
end ACPI_Firmware_Data_Transport;
