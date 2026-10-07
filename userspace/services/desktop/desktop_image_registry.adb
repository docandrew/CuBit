package body Desktop_Image_Registry is
   use type System.Address, Interfaces.Unsigned_64, I.Phase, I.V.Source_Slot,
     Compositor_Formats.Word, Compositor_Formats.Image;
   function Last_Pressure (S : State) return Capacity_Pressure is (S.Pressure);
   function Upload_Work (S : State) return Boolean is
     (for some N in I.V.Client_Slot =>
       I.Current (S.Items (N).Owner) in I.Uploading | I.Ready_To_Upload);
   function Faulted (S : State) return Boolean is
     (for some N in I.V.Client_Slot => I.Current (S.Items (N).Owner) = I.Quarantined);
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean) is
   begin
      Safe := False;
      if not Capture_Retired then return; end if;
      for N in I.V.Client_Slot loop
         if S.Items (N).Generation /= 0 then
            Forget (S, S.Items (N).Image.Pixels, True, Safe);
            if not Safe then return; end if;
         end if;
      end loop;
      Safe := True;
   end Close;
   procedure Ensure (S : in out State; Image : Compositor_Formats.Image;
      Bytes : Natural; Source : out I.V.Source_Ticket; Result : out I.Outcome) is
      Selected : I.V.Client_Slot := I.V.Client_Slot'First;
      Found : Boolean := False;
   begin
      Source := I.V.No_Source; Result := I.Rejected; S.Pressure := None;
      if Image.Writable /= 0 or else not Compositor_Formats.Valid
        (Image, Interfaces.Unsigned_64 (Bytes)) then return; end if;
      for N in I.V.Client_Slot loop
         if S.Items (N).Generation /= 0 and then S.Items (N).Image.Pixels = Image.Pixels then
            Selected := N; Found := True; exit;
         end if;
      end loop;
      if not Found then
         for N in I.V.Client_Slot loop
            if S.Items (N).Generation = 0 then Selected := N; Found := True; exit; end if;
         end loop;
         if not Found then
            S.Pressure := Slots_Full; Result := I.Deferred; return;
         elsif S.Serial = Interfaces.Unsigned_64'Last then
            S.Pressure := Generation_Exhausted; Result := I.Deferred; return;
         end if;
         S.Serial := S.Serial + 1;
         S.Items (Selected).Generation := S.Serial;
         S.Items (Selected).Image := Image; S.Items (Selected).Bytes := Bytes;
      end if;
      if S.Items (Selected).Closing then Result := I.Deferred; return; end if;
      if S.Items (Selected).Image /= Image or else S.Items (Selected).Bytes /= Bytes then return; end if;
      I.Acquire (S.Items (Selected).Owner, Selected, S.Items (Selected).Generation,
        Image, Bytes, Source, Result);
   end Ensure;
   procedure Poll (S : in out State; Result : out I.Outcome) is
      N : I.V.Client_Slot;
   begin
      Result := I.Deferred;
      for Attempt in I.V.Client_Slot loop
         N := S.Cursor;
         S.Cursor := (if N = I.V.Client_Slot'Last then I.V.Client_Slot'First else N + 1);
         if I.Current (S.Items (N).Owner) in I.Uploading | I.Ready_To_Upload then
            I.Poll (S.Items (N).Owner, Result); return;
         end if;
      end loop;
   end Poll;
   procedure Forget (S : in out State; Pixels : System.Address;
      Capture_Retired : Boolean; Safe : out Boolean) is
      Rearmed : Boolean;
   begin
      Safe := True;
      for N in I.V.Client_Slot loop
         if S.Items (N).Generation /= 0 and then S.Items (N).Image.Pixels = Pixels then
            S.Items (N).Closing := True;
            I.Close (S.Items (N).Owner, Capture_Retired, Safe);
            if not Safe then return; end if;
            I.Rearm (S.Items (N).Owner, Rearmed);
            Safe := Rearmed; if not Safe then return; end if;
            S.Items (N).Generation := 0; S.Items (N).Image := (others => <>);
            S.Items (N).Bytes := 0; S.Items (N).Closing := False;
            return;
         end if;
      end loop;
   end Forget;
end Desktop_Image_Registry;
