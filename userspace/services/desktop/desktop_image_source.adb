with Compositor_Upload_Copy;
with Compositor_Upload;
with System;
package body Desktop_Image_Source is
   use type D.Source_Result, D.Poll_Result, A.Ticket, V.Source_Ticket,
     V.Source_Slot, Interfaces.Unsigned_64, Compositor_Formats.Image,
     Compositor_Formats.Word, System.Address;
   procedure Rearm (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := S.Status = Closed and then S.Lease = A.No_Ticket and then S.Source = V.No_Source;
      if Accepted then S.Status := Fresh; end if;
   end Rearm;
   procedure Advance (S : in out State; Result : out Outcome) is
      Reply : D.Source_Result;
      Write : D.Write_Ticket;
      Plan : Compositor_Upload.Plan;
      Mapping : System.Address;
      Copied, Cancelled : Boolean;
   begin
      D.Import_Backing (S.Index, S.Lease, S.Source, Reply);
      if Reply = D.Source_Accepted and then S.Source /= V.No_Source then
         S.Status := Resident; Result := Available; return;
      end if;
      if Reply = D.Source_Busy then Result := Deferred; return; end if;
      if Reply = D.Source_Unsafe then S.Status := Quarantined; Result := Unsafe; return; end if;
      D.Begin_Write (S.Index, S.Lease, Write, Plan, Mapping, Reply);
      if Reply = D.Source_Accepted then
         Compositor_Upload_Copy.Copy (S.Image.Pixels, Mapping, S.Bytes,
           Natural (S.Image.Pitch), Natural (S.Image.Width), Natural (S.Image.Height),
           Compositor_Upload.BGRA8, Plan, Copied);
         if Copied then D.Submit_Write (Write, True, Reply);
         else
            D.Cancel_Write (Write, True, Cancelled);
            Reply := (if Cancelled then D.Source_Rejected else D.Source_Unsafe);
         end if;
      end if;
      case Reply is
         when D.Source_Accepted => S.Status := Uploading; Result := Pending;
         when D.Source_Busy => Result := Deferred;
         when D.Source_Rejected => Result := Rejected;
         when D.Source_Unsafe => S.Status := Quarantined; Result := Unsafe;
      end case;
   end Advance;
   procedure Acquire (S : in out State; Index : V.Client_Slot;
      Generation : Interfaces.Unsigned_64; Image : Compositor_Formats.Image;
      Bytes : Natural; Source : out V.Source_Ticket; Result : out Outcome) is
      Reply : D.Source_Result;
   begin
      Source := V.No_Source; Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status = Closed or else Generation = 0 or else Image.Writable /= 0 or else
         not Compositor_Formats.Valid (Image, Interfaces.Unsigned_64 (Bytes)) then return; end if;
      if S.Status = Fresh then
         D.Allocate_Backing (Index, Image.Width, Image.Height, False, S.Lease, Reply);
         if S.Lease /= A.No_Ticket then
            S.Index := Index; S.Generation := Generation; S.Image := Image; S.Bytes := Bytes;
            S.Status := (if Reply = D.Source_Accepted then Ready_To_Upload else Quarantined);
         end if;
         if Reply /= D.Source_Accepted or else S.Lease = A.No_Ticket then
            if Reply = D.Source_Unsafe then S.Status := Quarantined; Result := Unsafe;
            elsif Reply = D.Source_Busy then Result := Deferred; end if;
            return;
         end if;
      elsif S.Index /= Index or else S.Generation /= Generation or else
         S.Image /= Image or else S.Bytes /= Bytes then return;
      end if;
      case S.Status is
         when Ready_To_Upload => Advance (S, Result);
         when Uploading => Result := Pending;
         when others => null;
      end case;
      if S.Status = Resident then
         if D.Source_Held (S.Source) then Source := S.Source; Result := Available;
         else S.Status := Quarantined; Result := Unsafe; end if;
      end if;
   end Acquire;
   procedure Poll (S : in out State; Result : out Outcome) is
      Reply : D.Poll_Result;
   begin
      Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status = Resident then
         if D.Source_Held (S.Source) then Result := Available;
         else S.Status := Quarantined; Result := Unsafe; end if;
         return;
      end if;
      if S.Status = Uploading then
         D.Poll_Upload (Reply);
         case Reply is
            when D.Pending => Result := Pending; return;
            when D.Completed => S.Status := Ready_To_Upload;
            when others => S.Status := Quarantined; Result := Unsafe; return;
         end case;
      end if;
      if S.Status = Ready_To_Upload then Advance (S, Result); end if;
   end Poll;
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean) is
      Released : System.Address;
      Freed : Boolean;
   begin
      Safe := S.Status = Closed;
      if Safe or else not Capture_Retired or else S.Status in Uploading | Quarantined or else
         not D.Can_Retire_Readers then return; end if;
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
      S.Image := (others => <>); S.Generation := 0; S.Bytes := 0;
      S.Status := Closed; Safe := True;
   end Close;
end Desktop_Image_Source;
