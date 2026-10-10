with Compositor_Upload_Copy;
with Compositor_Upload;
with Interfaces;
package body Desktop_Image_Source with SPARK_Mode is
   use type D.Source_Result, D.Poll_Result, A.Ticket, V.Source_Ticket,
     V.Source_Slot, Interfaces.Unsigned_64, Compositor_Formats.Image,
     Compositor_Formats.Word, C.Content_Version;
   function Evictable (S : State) return Boolean is
     (S.Status in Fresh | Closed or else
      (S.Status = Resident and then D.Can_Retire_Readers and then
       not D.Source_Pinned (S.Source)));
   procedure Rearm (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := S.Status = Closed and then S.Lease = A.No_Ticket and then S.Source = V.No_Source;
      if Accepted then S.Status := Fresh; end if;
   end Rearm;
   procedure Quarantine (S : in out State; Result : out Outcome)
     with Pre => Valid (S),
       Post => Valid (S) and S.Status = Quarantined and Result = Unsafe is
   begin
      S.Status := Quarantined; Result := Unsafe;
   end Quarantine;
   -- Import a completed pass, or copy and submit its next row chunk.
   procedure Advance (S : in out State; Result : out Outcome)
     with Pre => Valid (S) and D.Valid and S.Status = Ready_To_Upload,
       Post => Valid (S) and D.Valid and
         S.Status in Ready_To_Upload | Uploading | Resident | Quarantined and
         (if Result in Available | Pending then S.Status in Uploading | Resident)
   is
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
      if Reply = D.Source_Unsafe then Quarantine (S, Result); return; end if;
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
         when D.Source_Unsafe => Quarantine (S, Result);
      end case;
   end Advance;
   -- Fresh slot: one allocation for this extent; its first pass copies every row.
   procedure Allocate (S : in out State; Index : V.Client_Slot;
      Version : C.Content_Version; Image : Compositor_Formats.Image; Bytes : Natural;
      Result : out Outcome)
     with Pre => Valid (S) and D.Valid and S.Status = Fresh and
                 Compositor_Formats.Valid (Image, Interfaces.Unsigned_64 (Bytes)),
       Post => Valid (S) and D.Valid and
         (if Result in Available | Pending then S.Status in Ready_To_Upload | Uploading | Resident)
   is
      Reply : D.Source_Result;
      Lease : A.Ticket;
   begin
      Result := Rejected;
      D.Allocate_Backing (Index, Image.Width, Image.Height, False, Lease, Reply);
      if Lease /= A.No_Ticket then
         S.Index := Index; S.Lease := Lease; S.Width := Image.Width; S.Height := Image.Height;
         S.Held := Version; S.Image := Image; S.Bytes := Bytes;
         S.Status := (if Reply = D.Source_Accepted then Ready_To_Upload else Quarantined);
      end if;
      if Reply /= D.Source_Accepted or else Lease = A.No_Ticket then
         case Reply is
            when D.Source_Unsafe =>
               if S.Status /= Fresh then S.Status := Quarantined; end if;
               Result := Unsafe;
            when D.Source_Busy => Result := Deferred;
            when D.Source_Rejected => Result := Unaffordable;
            when D.Source_Accepted => if S.Status /= Fresh then S.Status := Quarantined; end if;
               Result := Unsafe;
         end case;
         return;
      end if;
      Advance (S, Result);
   end Allocate;
   procedure Acquire (S : in out State; Index : V.Client_Slot;
      Version : C.Content_Version; Image : Compositor_Formats.Image;
      Bytes : Natural; Stale : C.Row_Band; Started : out Boolean;
      Source : out V.Source_Ticket; Result : out Outcome) is
      Released : System.Address;
      OK : Boolean;
      Rows : C.Row_Band;
   begin
      Source := V.No_Source; Started := False; Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status = Closed or else Version = C.No_Version or else Image.Writable /= 0 or else
         not Compositor_Formats.Valid (Image, Interfaces.Unsigned_64 (Bytes)) or else
         Image.Height > C.Maximum_Rows or else
         (S.Status /= Fresh and then S.Index /= Index) then return; end if;
      case S.Status is
         when Fresh =>
            Allocate (S, Index, Version, Image, Bytes, Result);
            Started := S.Status /= Fresh;
         when Resident =>
            if Version = S.Held and then Image.Width = S.Width and then Image.Height = S.Height then
               null;
            elsif not D.Can_Retire_Readers or else D.Source_Pinned (S.Source) then
               -- A reader of the held version is still live; keep it intact.
               Result := Deferred; return;
            else
               D.Release_Source (S.Source, Released);
               if Released = System.Null_Address then Quarantine (S, Result); return; end if;
               S.Source := V.No_Source;
               if Image.Width /= S.Width or else Image.Height /= S.Height then
                  -- Only an extent change replaces the allocation.
                  D.Release_Backing (Index, S.Lease, OK);
                  if not OK then Quarantine (S, Result); return; end if;
                  S.Lease := A.No_Ticket; S.Status := Fresh;
                  S.Width := 0; S.Height := 0; S.Held := C.No_Version;
                  Allocate (S, Index, Version, Image, Bytes, Result);
                  Started := S.Status /= Fresh;
               else
                  Rows := C.Clip (Stale, Natural (Image.Height));
                  if C.Is_Empty (Rows) then
                     D.Restart_Content (Index, S.Lease, OK);
                  else
                     D.Update_Content (Index, S.Lease, Rows.First, Rows.Last, OK);
                  end if;
                  if not OK then Quarantine (S, Result); return; end if;
                  S.Held := Version; S.Image := Image; S.Bytes := Bytes;
                  S.Status := Ready_To_Upload; Started := True;
                  Advance (S, Result);
               end if;
            end if;
         when Ready_To_Upload =>
            -- Finish the pass in flight first; a newer version waits for it.
            Advance (S, Result);
            if Result = Available then Result := Pending; end if;
         when Uploading => Result := Pending;
         when Quarantined | Closed => null;
      end case;
      if S.Status = Resident and then Version = S.Held and then
         Image.Width = S.Width and then Image.Height = S.Height
      then
         if D.Source_Held (S.Source) then Source := S.Source; Result := Available;
         else Quarantine (S, Result); end if;
      elsif Result = Available then
         -- Resident, but with an older version or extent: still cold.
         Result := Pending;
      end if;
   end Acquire;
   procedure Poll (S : in out State; Result : out Outcome) is
      Reply : D.Poll_Result;
   begin
      Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status = Resident then
         if D.Source_Held (S.Source) then Result := Available;
         else Quarantine (S, Result); end if;
         return;
      end if;
      if S.Status = Uploading then
         D.Poll_Upload (Reply);
         case Reply is
            when D.Pending => Result := Pending; return;
            when D.Completed => S.Status := Ready_To_Upload;
            when others => Quarantine (S, Result); return;
         end case;
      end if;
      if S.Status = Ready_To_Upload then Advance (S, Result); end if;
   end Poll;
   procedure Detach (S : in out State; Pixels : System.Address; Safe : out Boolean) is
   begin
      Safe := not Reads (S, Pixels) and not
        (S.Status = Quarantined and S.Image.Pixels = Pixels);
      if Safe and then S.Image.Pixels = Pixels and then S.Status not in Ready_To_Upload | Uploading then
         S.Image := (others => <>); S.Bytes := 0;
      end if;
   end Detach;
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
      S.Image := (others => <>); S.Held := C.No_Version; S.Bytes := 0;
      S.Width := 0; S.Height := 0;
      S.Status := Closed; Safe := True;
   end Close;
end Desktop_Image_Source;
