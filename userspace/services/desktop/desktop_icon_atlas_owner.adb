with Desktop_Icon_Pixels.Atlases;
with Desktop_Icon_Upload;
with Interfaces;
with System;
package body Desktop_Icon_Atlas_Owner with SPARK_Mode is
   use type V.Source_Slot, D.Source_Result, D.Poll_Result, Desktop_Icon_Pixels.Family, System.Address;
   procedure Advance (S : in out State; Result : out Outcome)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid and Current (S) = Ready_To_Upload,
       Post => Valid (S) and D.Valid and (if Result = Available then Current (S) = Resident)
   is
      Reply : D.Source_Result;
      Source : V.Source_Ticket;
   begin
      D.Import_Backing (S.Index, S.Lease, Source, Reply);
      if Reply = D.Source_Accepted and Source /= V.No_Source then
         S.Source := Source; S.Status := Resident; Result := Available; return;
      elsif Reply = D.Source_Unsafe then S.Status := Quarantined; Result := Unsafe; return;
      elsif Reply = D.Source_Busy then Result := Deferred; return;
      end if;
      Desktop_Icon_Upload.Start_Atlas (S.Index, S.Lease, S.Asset, Reply);
      case Reply is
         when D.Source_Accepted => S.Status := Uploading; Result := Pending;
         when D.Source_Busy => Result := Deferred;
         when D.Source_Rejected => Result := Rejected;
         when D.Source_Unsafe => S.Status := Quarantined; Result := Unsafe;
      end case;
   end Advance;
   procedure Acquire
     (S : in out State; Index : Slot; Asset : Desktop_Icon_Pixels.Family;
      Source : out V.Source_Ticket; Result : out Outcome)
   is
      Reply : D.Source_Result;
      Lease : A.Ticket;
   begin
      Source := V.No_Source; Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status = Closed then return; end if;
      if S.Status = Fresh then
         D.Allocate_Backing (Index,
           Interfaces.Unsigned_32 (Desktop_Icon_Pixels.Atlases.Width (Asset)),
           Interfaces.Unsigned_32 (Desktop_Icon_Pixels.Atlases.Height (Asset)), False, Lease, Reply);
         if Lease /= A.No_Ticket then
            S.Index := Index; S.Asset := Asset; S.Lease := Lease;
            S.Status := (if Reply = D.Source_Accepted then Ready_To_Upload else Quarantined);
         end if;
         if Reply /= D.Source_Accepted or Lease = A.No_Ticket then
            if Reply = D.Source_Unsafe then S.Status := Quarantined; Result := Unsafe;
            elsif Reply = D.Source_Busy then Result := Deferred; end if;
            return;
         end if;
      elsif S.Index /= Index or S.Asset /= Asset then return;
      end if;
      if S.Status = Ready_To_Upload then Advance (S, Result); end if;
      if S.Status = Uploading then Result := Pending;
      elsif S.Status = Resident then
         if D.Source_Held (S.Source) then Source := S.Source; Result := Available;
         else S.Status := Quarantined; Result := Unsafe; end if;
      end if;
   end Acquire;
   procedure Poll (S : in out State; Result : out Outcome) is
      Reply : D.Poll_Result;
   begin
      Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status = Resident then Result := Available; return; end if;
      if S.Status = Uploading then
         D.Poll_Upload (Reply);
         case Reply is
            when D.Pending => Result := Pending; return;
            when D.Completed => S.Status := Ready_To_Upload;
            when D.GPU_Failed | D.Idle => S.Status := Quarantined; Result := Unsafe; return;
         end case;
      end if;
      if S.Status = Ready_To_Upload then Advance (S, Result); end if;
   end Poll;
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean) is
      Released : System.Address;
      Freed : Boolean;
   begin
      Safe := S.Status = Closed;
      if Safe then return; end if;
      if not Capture_Retired or else S.Status in Uploading | Quarantined or else not D.Can_Retire_Readers then return; end if;
      if S.Source /= V.No_Source then
         D.Release_Source (S.Source, Released);
         if Released = System.Null_Address then S.Status := Quarantined; return; end if;
         S.Source := V.No_Source;
      end if;
      if S.Lease /= A.No_Ticket then
         D.Release_Backing (S.Index, S.Lease, Freed);
         if not Freed then S.Status := Quarantined; return; end if;
         S.Lease := A.No_Ticket;
      end if;
      S.Status := Closed; Safe := True;
   end Close;
end Desktop_Icon_Atlas_Owner;
