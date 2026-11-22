package body Compositor_Input_Delivery with SPARK_Mode is
   use type W.Word;
   procedure Retire (S : in out State) is
      Confirmed : Boolean;
   begin
      if S.Held then
         Return_Loan (S.Loan, Confirmed);
         if Confirmed then S.Held := False; end if;
      end if;
   end Retire;

   procedure Deliver
     (S : in out State; Owner : W.Identity; Grant : GR.Reference;
      Payload : W.Snapshot_Words; Result : out Outcome)
   is
      Mapping : W.Word;
      Acquired, Written : Boolean;
   begin
      if S.Held then Result := Busy; return; end if;
      Acquire (Owner, Grant, Mapping, Acquired);
      if not Acquired then Result := Acquisition_Failed; return; end if;
      -- Remember ownership before invoking the writer, even for an invalid
      -- null mapping returned by the boundary. Such a loan still needs return.
      S.Loan := Grant;
      S.Held := True;
      Written := False;
      if Mapping /= 0 then Write (Mapping, Payload, Written); end if;
      Retire (S);
      if S.Held then Result := Quarantined;
      elsif Written then Result := Published;
      else Result := Publication_Failed;
      end if;
   end Deliver;
end Compositor_Input_Delivery;
