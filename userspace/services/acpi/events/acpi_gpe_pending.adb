pragma Ada_2022;
package body ACPI_GPE_Pending with SPARK_Mode,
   Refined_State => (Queue_State => (Stage, Allowed, Waiting, Cursor, Serial, Current, Has_Current, Broken)) is
   use type Interfaces.Unsigned_64;
   Stage : Phase := Fresh;
   Allowed, Waiting : Source_Set := No_Sources;
   Cursor : Source_Number := Source_Number'First;
   Delivery_Limit : constant Interfaces.Unsigned_64 := Maximum_Delivery_ID;
   subtype Delivery_Sequence is Interfaces.Unsigned_64 range 0 .. Delivery_Limit;
   Serial : Delivery_Sequence := 0;
   Current : Ticket := No_Ticket;
   Has_Current, Broken : Boolean := False;
   function Next_Delivery (Value, Ceiling : Interfaces.Unsigned_64) return Interfaces.Unsigned_64
     with Global => null, Pre => Value < Ceiling,
       Post => Next_Delivery'Result > Value and then Next_Delivery'Result <= Ceiling;
   function Next_Delivery (Value, Ceiling : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
     (Value + 1);

   function Is_Issued (Item : Ticket) return Boolean is (Item.ID /= 0);
   function Source (Item : Ticket) return Source_Number is (Item.Number);
   function Following (Number : Source_Number) return Source_Number is
     (if Number = Source_Number'Last then Source_Number'First else Source_Number'Succ (Number));
   function Number_Pending return Pending_Count is
      Count : Pending_Count := 0;
   begin
      for Number in Source_Number loop
         pragma Loop_Invariant (Count <= Source_Number'Pos (Number));
         if Waiting (Number) then Count := Count + 1; end if;
      end loop;
      return Count;
   end Number_Pending;
   function Observe return View is ((Stage, Number_Pending, Has_Current, Broken));
   function Enrolled (Number : Source_Number) return Boolean is (Allowed (Number));
   function Pending (Number : Source_Number) return Boolean is (Waiting (Number));
   procedure Start (Sources : Source_Set; Accepted : out Boolean) is
   begin
      Accepted := Stage = Fresh;
      if not Accepted then return; end if;
      Allowed := Sources; Stage := Accepting;
   end Start;
   procedure Capture (Number : Source_Number; Result : out Capture_Result) is
   begin
      if Stage /= Accepting then Result := Not_Accepting;
      elsif not Allowed (Number) then Result := Not_Enrolled;
      elsif Has_Current and then Current.Number = Number then Result := Source_Active;
      elsif Waiting (Number) then Result := Coalesced;
      else Waiting (Number) := True; Result := Captured;
      end if;
   end Capture;
   procedure Claim (Item : out Ticket; Result : out Claim_Result) is
      Candidate : Source_Number := Cursor;
   begin
      Item := No_Ticket;
      if Stage /= Accepting then Result := Closed; return; end if;
      if Has_Current then Result := Busy; return; end if;
      if Number_Pending = 0 then Result := Empty; return; end if;
      if Serial = Delivery_Limit then
         Stage := Faulted; Broken := True; Result := Exhausted; return;
      end if;
      for Turn in Source_Number loop
         if Waiting (Candidate) then
            Serial := Delivery_Sequence (Next_Delivery (Serial, Delivery_Limit));
            Current := (ID => Serial, Number => Candidate); Has_Current := True;
            Waiting (Candidate) := False; Cursor := Following (Candidate);
            Item := Current; Result := Claimed; return;
         end if;
         Candidate := Following (Candidate);
      end loop;
      -- All candidates were inspected; an empty result owns no new receipt.
      Result := Empty;
   end Claim;
   procedure Finish (Item : Ticket; Healthy : Boolean; Result : out Finish_Result) is
   begin
      if not Has_Current or else Item /= Current then Result := Stale; return; end if;
      if Stage not in Accepting | Stopping then Result := Closed; return; end if;
      if not Healthy then
         Stage := Faulted; Broken := True; Result := Failure_Retained; return;
      end if;
      Has_Current := False; Current := No_Ticket; Result := Completed;
      if Stage = Stopping and then Number_Pending = 0 then Stage := Stopped; end if;
   end Finish;
   procedure Begin_Stop is
   begin
      if Stage in Fresh | Accepting then Stage := Stopping; end if;
      if Stage = Stopping and then not Has_Current and then Number_Pending = 0 then Stage := Stopped; end if;
   end Begin_Stop;
   procedure Discard_Queued (Count : out Pending_Count) is
   begin
      Count := 0;
      if Stage not in Stopping | Faulted then return; end if;
      Count := Number_Pending; Waiting := No_Sources;
      if not Has_Current then Stage := Stopped; end if;
   end Discard_Queued;
   procedure Retire_Active (Item : Ticket; Result : out Retire_Result) is
   begin
      if Stage not in Stopping | Faulted then Result := Not_Stopping; return; end if;
      if not Has_Current or else Item /= Current then Result := Stale; return; end if;
      Has_Current := False; Current := No_Ticket; Result := Retired;
      if Number_Pending = 0 then Stage := Stopped; end if;
   end Retire_Active;
end ACPI_GPE_Pending;
