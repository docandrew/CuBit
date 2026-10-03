with Desktop_Glyph_Upload;
with Interfaces;
with System;
package body Desktop_Glyph_Residency with SPARK_Mode is
   use type D.Source_Result, D.Poll_Result, System.Address, Vulkan_Owned_Targets.I.Phase;
   function Index (Slot : C.Slot) return D.Backing_Slot is (D.Backing_Slot (Slot - 1));
   function Cache_Key (Key : Vulkan_Glyph_Sources.Key) return C.Key is ((Key.Face, Key.Code, Key.Scale));
   procedure Retire (S : in out State; T : C.Token; OK : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid and S.Stage = Idle,
       Post => Valid (S) and D.Valid and S.Stopping = S.Stopping'Old and (if OK then S.Stage = Idle)
   is
      Retired : System.Address;
      Position : constant C.Slot := T.Position;
   begin
      C.Begin_Retirement (S.Cache, T, OK);
      if not OK then return; end if;
      if D.Source_Held (S.Sources (Position)) then
         D.Release_Source (S.Sources (Position), Retired);
         if Retired = System.Null_Address then S.Stage := Quarantined; OK := False; return; end if;
      end if;
      S.Sources (Position) := V.No_Source;
      if S.Backings (Position) /= A.No_Ticket then
         D.Release_Backing (Index (Position), S.Backings (Position), OK);
      else
         OK := D.Backing_Phase (Index (Position)) in Vulkan_Owned_Targets.I.Fresh | Vulkan_Owned_Targets.I.Closed;
      end if;
      if not OK then S.Stage := Quarantined; return; end if;
      S.Backings (Position) := A.No_Ticket;
      C.Retired (S.Cache, T, True);
   end Retire;
   procedure Acquire (S : in out State; Key : Vulkan_Glyph_Sources.Key;
      Source : out V.Source_Ticket; Reader : out C.Lease; Result : out Outcome) is
      Found : C.Search_Result;
      T : C.Token;
      OK : Boolean;
      Native : D.Source_Result;
      Advance : Natural;
      Layout : constant C.L.Layout := C.L.Plan (Key.Scale);
   begin
      Source := V.No_Source; Reader := C.No_Lease; Result := Deferred;
      if S.Stopping then Result := Rejected; return; end if;
      if S.Stage = Quarantined then Result := Unsafe; return; end if;
      if S.Stage = Transferring then Result := Uploading; return; end if;
      Found := C.Find (S.Cache, Cache_Key (Key));
      if Found /= 0 then
         T := C.At_Slot (S.Cache, Found);
         if not C.Current (S.Cache, T) or else C.Status (S.Cache, T) /= C.Ready or else
            S.Sources (Found) = V.No_Source or else D.Glyph_Source (Key) /= S.Sources (Found)
         then S.Stage := Quarantined; Result := Unsafe; return; end if;
         C.Acquire (S.Cache, T, Reader);
         if Reader /= C.No_Lease then Source := S.Sources (Found); Result := Available; end if;
         return;
      end if;
      if not D.Can_Retire_Readers then return; end if;
      C.Reserve (S.Cache, Cache_Key (Key), T);
      if T = C.No_Token then
         Found := C.Victim (S.Cache);
         if Found = 0 then return; end if;
         Retire (S, C.At_Slot (S.Cache, Found), OK);
         if not OK then Result := Unsafe; return; end if;
         C.Reserve (S.Cache, Cache_Key (Key), T);
         if T = C.No_Token then return; end if;
      end if;
      D.Allocate_Backing (Index (T.Position), Interfaces.Unsigned_32 (Layout.Width),
         Interfaces.Unsigned_32 (Layout.Height), True, S.Backings (T.Position), Native);
      if Native /= D.Source_Accepted then
         if Native = D.Source_Unsafe then S.Stage := Quarantined; Result := Unsafe;
         else
            -- A rejected setup can still own a native object. Record its actual
            -- lease and require confirmed retirement before refunding cache quota.
            S.Backings (T.Position) := D.Backing_Lease (Index (T.Position));
            Retire (S, T, OK); Result := (if OK then Rejected else Unsafe);
         end if;
         return;
      end if;
      if S.Backings (T.Position) = A.No_Ticket then
         S.Stage := Quarantined; Result := Unsafe; return;
      end if;
      Desktop_Glyph_Upload.Start (Index (T.Position), S.Backings (T.Position), Key, Advance, Native);
      if Native = D.Source_Accepted then
         S.Active := T; S.Key := Key; S.Stage := Transferring; Result := Uploading;
      elsif Native = D.Source_Unsafe then S.Stage := Quarantined; Result := Unsafe;
      else Retire (S, T, OK); Result := (if OK then Rejected else Unsafe);
      end if;
   end Acquire;
   procedure Poll (S : in out State; Result : out Outcome) is
      Observed : D.Poll_Result;
      Native : D.Source_Result;
      OK : Boolean;
   begin
      Result := Deferred;
      if S.Stage = Quarantined then Result := Unsafe; return; end if;
      if S.Stage /= Transferring then return; end if;
      D.Poll_Upload (Observed);
      if Observed = D.Pending then Result := Uploading; return; end if;
      if Observed /= D.Completed then S.Stage := Quarantined; Result := Unsafe; return; end if;
      D.Import_Backing (Index (S.Active.Position), S.Backings (S.Active.Position), S.Sources (S.Active.Position), Native);
      if Native /= D.Source_Accepted then S.Stage := Quarantined; Result := Unsafe; return; end if;
      D.Bind_Glyph (Index (S.Active.Position), S.Backings (S.Active.Position), S.Key, S.Sources (S.Active.Position), OK);
      if not OK then S.Stage := Quarantined; Result := Unsafe; return; end if;
      S.Stage := Idle;
      C.Publish (S.Cache, S.Active, True);
      S.Active := C.No_Token;
      Result := Available;
   end Poll;
   procedure Release (S : in out State; Reader : C.Lease; Capture_Retired : Boolean) is
   begin
      if S.Stage = Closed then return; end if;
      C.Complete (S.Cache, Reader, Capture_Retired and D.Can_Retire_Readers);
   end Release;
   procedure Close (S : in out State; Safe : out Boolean) is
      T : C.Token;
      OK : Boolean;
   begin
      Safe := False; S.Stopping := True;
      if S.Stage = Closed then Safe := True; return; end if;
      if S.Stage /= Idle then return; end if;
      if C.Charged (S.Cache) = 0 and C.Reader_Count (S.Cache) = 0 then
         S.Stage := Closed; Safe := True; return;
      end if;
      if not D.Can_Retire_Readers then return; end if;
      for Position in C.Slot loop
         T := C.At_Slot (S.Cache, Position);
         if C.Current (S.Cache, T) then
            if C.Pinned (S.Cache, Position) then return; end if;
            Retire (S, T, OK); if not OK then return; end if;
         end if;
         pragma Loop_Invariant (Valid (S) and D.Valid and S.Stage = Idle and S.Stopping);
      end loop;
      Safe := C.Charged (S.Cache) = 0 and C.Reader_Count (S.Cache) = 0;
      if Safe then S.Stage := Closed; end if;
   end Close;
end Desktop_Glyph_Residency;
