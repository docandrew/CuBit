pragma Ada_2022;
with AML_Identity.Issuer;
package body ACPI_Software_Global_Lock with SPARK_Mode is
   function Observe (State : Context) return Report is (State.Info);
   function Needs_Cleanup (Item : Lease) return Boolean is
     (Item.Claim /= AML_Identity.No_Identity);
   function Owns (State : Context; Item : Lease) return Boolean is
     (State.Info.Held and then State.Claim /= AML_Identity.No_Identity
      and then Item.Claim = State.Claim);
   function Claim_Of (Item : Lease) return AML_Identity.Identity is (Item.Claim);
   function Claim_Of (State : Context) return AML_Identity.Identity is (State.Claim);
   function Deadline_Of (Item : Lease) return Microseconds is (Item.Deadline);
   procedure Configure (State : in out Context; Facts : ACPI_FADT.Description;
      Result : out P.Lock_Outcome) is
   begin
      Result := P.Invalid_Configuration;
      if State.Attempted or else State.Info.Held then Result := P.Already_Configured; return; end if;
      State.Attempted := True;
      if State.Info.Stopping or else not ACPI_FADT.Hardware_Reduced (Facts) then return; end if;
      State.Info.Configured := True; Result := P.Configured;
   end Configure;
   procedure Clock (State : in out Context; Now : Microseconds;
      Available : Boolean; Result : out P.Lock_Outcome) is
   begin
      Result := P.Clock_Failed;
      if State.Info.Clock_Broken or else not Available then return; end if;
      if State.Seen_Clock and then Now < State.Last_Clock then
         State.Info.Clock_Broken := True; Result := P.Clock_Reversed; return;
      end if;
      State.Seen_Clock := True; State.Last_Clock := Now; Result := P.Configured;
   end Clock;
   procedure Acquire (State : in out Context; Item : in out Lease;
      Now : Microseconds; Clock_Available : Boolean; Deadline : Microseconds;
      Result : out P.Lock_Outcome) is
      Claim : AML_Identity.Identity;
      Issued : Boolean;
   begin
      Result := P.Not_Ready;
      if not State.Info.Configured or else State.Info.Stopping then return; end if;
      if Needs_Cleanup (Item) then Result := P.Already_Held; return; end if;
      if State.Info.Held then Result := P.Contended; return; end if;
      if Deadline in 0 | Microseconds'Last then Result := P.Invalid_Deadline; return; end if;
      Clock (State, Now, Clock_Available, Result);
      if Result /= P.Configured then return; end if;
      if Now >= Deadline then Result := P.Expired; return; end if;
      AML_Identity.Issuer.Issue (Claim, Issued);
      if not Issued then Result := P.Lease_Exhausted; return; end if;
      State.Claim := Claim; State.Info.Held := True;
      Item.Claim := Claim; Item.Deadline := Deadline; Result := P.Acquired;
   end Acquire;
   procedure Check_Access (State : in out Context; Item : Lease;
      Now : Microseconds; Clock_Available : Boolean; Result : out P.Lock_Outcome) is
   begin
      Result := P.Stale_Lease;
      if not Owns (State, Item) then return; end if;
      if not State.Info.Configured or else State.Info.Stopping then Result := P.Not_Ready; return; end if;
      Clock (State, Now, Clock_Available, Result);
      if Result = P.Configured and then Now >= Item.Deadline then Result := P.Expired; end if;
   end Check_Access;
   procedure Release (State : in out Context; Item : in out Lease;
      Result : out P.Lock_Outcome) is
   begin
      Result := P.Stale_Lease;
      if not Owns (State, Item) then return; end if;
      State.Claim := AML_Identity.No_Identity; State.Info.Held := False;
      Item.Claim := AML_Identity.No_Identity; Item.Deadline := 0; Result := P.Released;
   end Release;
   procedure Begin_Stop (State : in out Context) is
   begin State.Info.Stopping := True; end Begin_Stop;
end ACPI_Software_Global_Lock;
