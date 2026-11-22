pragma Ada_2022;
package body ACPI_Power_Entry with SPARK_Mode is
   use type Word, S.Outcome, S.Completion, S.Register_Model;
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   Enter_Microseconds : constant Word := Word (E.Enter_Budget_Milliseconds) * 1_000;
   function Observe (Item : State) return Report is (Item.Info);
   function Runnable (Item : State) return Boolean is
     (not Item.Busy and then (Item.Info.Stage in Running | Reply_Pending or else
       (Item.Stopping and then Item.Info.Stage = Settling)));
   function Blocks_Runtime (Item : State) return Boolean is (Item.Info.Attempted or Item.Stopping);
   function Quiet (Item : State) return Boolean is
     (not Item.Busy and then Item.Token = No_Reply and then not Item.Info.Uncertain_Effect
      and then Item.Info.Stage in Fresh | Finished);
   function Wake_Deadline_Milliseconds (Item : State) return Word is
     (if Item.Info.Stage = Settling and then not Item.Stopping then
        (if Item.Settle_Clock_Valid then Word'Min (Item.Until_Milliseconds, Item.Settle_Milliseconds)
         else Item.Until_Milliseconds)
      else No_Deadline);
   procedure Begin_Stop (Item : in out State) is
   begin
      Item.Stopping := True;
      if Item.Info.Stage = Fresh then Item.Info.Stage := Finished; end if;
   end Begin_Stop;
   procedure Clocks (Item : in out State; Microseconds, Milliseconds : out Word; Good : out Boolean) is
      Micro_OK, Milli_OK : Boolean;
   begin
      Read_Clock (Microseconds, Micro_OK); Read_Wait_Clock (Milliseconds, Milli_OK);
      Good := Micro_OK and then Milli_OK and then Microseconds /= Word'Last and then Milliseconds /= Word'Last
        and then (not Item.Clock_Seen or else
          (Microseconds >= Item.Last_Microseconds and then Milliseconds >= Item.Last_Milliseconds));
      if Good then Item.Clock_Seen := True; Item.Last_Microseconds := Microseconds; Item.Last_Milliseconds := Milliseconds; end if;
   end Clocks;
   procedure Fail (Item : in out State) is
   begin Item.Info.Reply.Status := E.Write_Refused; Item.Info.Stage := Reply_Pending; end Fail;
   procedure Enter (Item : in out State; Service : Service_State; Value : Request;
      Reply : out Response; Deferred : out Boolean) is
      Available, Good, Saved : Boolean;
      Model : S.Register_Model; A, B : E.Sleep_Type;
      Microseconds, Milliseconds : Word;
      Token : Reply_Token;
   begin
      Reply := (ID => Value.ID, Status => E.Not_Prepared); Deferred := False;
      if Value.Stamp /= E.Authority_Tag then Reply.Status := E.Enter_Not_Approved; return; end if;
      if Value.Flags /= 0 or else Value.Reserved /= 0 or else Value.Unused /= 0
        or else not E.Sleep_Request_Valid (Value.Length, Value.ID, Value.State, Value.Confirm)
      then Reply.Status := E.Enter_Malformed; return; end if;
      if Value.State /= Word (E.Sleep_State'Enum_Rep (E.S5))
        or else Item.Busy or else Item.Stopping or else Item.Info.Stage /= Fresh
      then return; end if;
      Item.Busy := True;
      Read_Prepared (Service, Available, Model, A, B);
      if not Available then Item.Busy := False; return; end if;
      if Model = S.No_Registers then Reply.Status := E.No_Sleep_Registers; Item.Busy := False; return; end if;
      if not Owner_Ready (Service) then Reply.Status := E.Write_Refused; Item.Busy := False; return; end if;
      Clocks (Item, Microseconds, Milliseconds, Good);
      if not Good or else Microseconds >= Word'Last - Enter_Microseconds
        or else Milliseconds >= Word'Last - Word (E.Enter_Budget_Milliseconds)
      then Reply.Status := E.Write_Refused; Item.Busy := False; return; end if;
      Save_Current (Token, Saved);
      if not Saved then Reply.Status := E.Write_Refused; Item.Busy := False; return; end if;
      -- Implicit reply authority is gone after Save_Current, even if the
      -- trusted callback violates its token contract. Never reply twice.
      Deferred := True; Item.Token := Token; Item.Info.Attempted := True;
      Item.Info.Reply := (ID => Value.ID, Status => E.Write_Refused);
      if Token = No_Reply then
         Item.Info.Stage := Reply_Contract_Unknown; Item.Busy := False; return;
      end if;
      Item.Until_Microseconds := Microseconds + Enter_Microseconds;
      Item.Until_Milliseconds := Milliseconds + Word (E.Enter_Budget_Milliseconds);
      Consume_Prepared;
      S.Start (Item.Sequence, True, Model, A, B, True, True, Microseconds, Item.Until_Microseconds, Item.Info.Sequence);
      Item.Info.Stage := (if Item.Info.Sequence.Status = S.Ready then Running else Reply_Pending);
      Item.Busy := False;
   end Enter;
   procedure Advance (Item : in out State; Service : Service_State; Result : out Delivery) is
      Microseconds, Milliseconds : Word;
      Good, Available, Delivered : Boolean;
      Command : S.Command; Output : S.Control_Word; Completion : S.Completion;
   begin
      Result := No_Answer;
      if Item.Busy or else Item.Info.Stage not in Running | Settling | Reply_Pending then return; end if;
      Item.Busy := True;
      if Item.Info.Stage = Reply_Pending then
         Answer_Saved (Item.Token, Item.Info.Reply.ID, Item.Info.Reply.Status, Delivered);
         Item.Token := No_Reply; Item.Info.Stage := Finished;
         Result := (if Delivered then Answered else Caller_Gone); Item.Info.Last_Delivery := Result;
         Item.Busy := False; return;
      end if;
      if Item.Stopping then Fail (Item); Item.Busy := False; return; end if;
      Clocks (Item, Microseconds, Milliseconds, Good);
      if not Good then Fail (Item); Item.Busy := False; return; end if;
      -- Independent IPC wait clock bounds settle even if the high-resolution
      -- clock stalls; no assumption about a shared absolute epoch.
      if Item.Info.Stage = Settling and then Item.Settle_Clock_Valid
        and then Milliseconds >= Item.Settle_Milliseconds
      then
         Item.Info.Reply.Status := E.Still_Running; Item.Info.Stage := Reply_Pending;
         Item.Busy := False; return;
      elsif Milliseconds >= Item.Until_Milliseconds then Fail (Item); Item.Busy := False; return;
      end if;
      if Item.Info.Stage = Settling then
         -- Public settle interval is measured by the IPC clock alone. The
         -- high-resolution clock may have another epoch/rate; no more
         -- hardware or AML is needed after the final confirmed enable.
         Item.Busy := False; return;
      end if;
      S.Take (Item.Sequence, Owner_Ready (Service), Good, Microseconds, Command, Available, Item.Info.Sequence);
      if Available then
         Execute_Command (Command, Item.Until_Microseconds, Output, Completion);
         Clocks (Item, Microseconds, Milliseconds, Good);
         S.Confirm (Item.Sequence, Completion, Output, Good, Microseconds, Item.Info.Sequence);
         Item.Info.Uncertain_Effect := Item.Info.Uncertain_Effect or Completion = S.Indeterminate;
      end if;
      case Item.Info.Sequence.Status is
         when S.Ready => Item.Info.Stage := Running;
         when S.Settling =>
            if Item.Info.Stage /= Settling then
               Item.Settle_Clock_Valid := Milliseconds < Word'Last - Word (E.Settle_Milliseconds);
               if Item.Settle_Clock_Valid then Item.Settle_Milliseconds := Milliseconds + Word (E.Settle_Milliseconds); end if;
            end if;
            Item.Info.Stage := Settling;
         when S.Still_Running => Item.Info.Reply.Status := E.Still_Running; Item.Info.Stage := Reply_Pending;
         when others => Fail (Item);
      end case;
      Item.Busy := False;
   end Advance;
end ACPI_Power_Entry;
