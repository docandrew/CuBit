package body Intel_GPU_Metadata_Bundle is
   use type Storage.State;
   function State (Object : Bundle) return Phase is (Object.Status);
   procedure Request
     (Object : in out Bundle; Count, Record_Quota : Positive;
      Per_Table_Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Request (Object, Count,
        [others => (Count, Record_Quota, Per_Table_Bytes)], Accepted);
   end Request;
   procedure Request
     (Object : in out Bundle; Count : Positive; Demand : Requirements;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Status /= Idle or else not Owner_Ready then return; end if;
      for T in Table_ID loop
         if Demand (T).Records > Demand (T).Record_Quota or else
           Demand (T).Byte_Quota = 0 or else
           Demand (T).Byte_Quota mod Storage.Page_Bytes /= 0 or else
           (Object.Plan (T).Byte_Quota /= 0 and then
            Object.Plan (T).Byte_Quota /= Demand (T).Byte_Quota)
         then return; end if;
      end loop;
      Object.Target := Count;
      Object.Plan := Demand;
      Object.Current := Table_ID'First;
      Object.Status := Checking;
      Accepted := True;
   end Request;
   procedure Step (Object : in out Bundle) is
      OK : Boolean;
      V : Storage.View;
   begin
      if Object.Status in Idle | Failed then return; end if;
      if not Owner_Ready then Object.Status := Failed; return; end if;
      V := Storage.Snapshot (Object.Items (Object.Current));
      case Object.Status is
         when Checking =>
            if Capacity (Object.Current) >= Object.Plan (Object.Current).Records then
               if Object.Current = Table_ID'Last then Object.Status := Admitting;
               else Object.Current := Table_ID'Succ (Object.Current); end if;
            else
               Object.Status := (if V.Phase = Storage.Empty then Opening else Requesting);
            end if;
         when Opening =>
            Object.Status := Failed;
            Storage.Open (Object.Items (Object.Current), Object.Plan (Object.Current).Byte_Quota, OK);
            if OK then Object.Status := Requesting; end if;
         when Requesting =>
            Object.Status := Failed;
            if V.Published >= Object.Plan (Object.Current).Byte_Quota then return; end if;
            Storage.Request (Object.Items (Object.Current), V.Published +
              Unsigned_64'Min
                (Unsigned_64'Min (Storage.Step_Bytes,
                   Unsigned_64'Max (Storage.Page_Bytes, V.Published)),
                 Object.Plan (Object.Current).Byte_Quota - V.Published), OK);
            -- A large virtual reservation is not demand for physical backing.
            -- Publish one page first, recheck actual capacity, then double
            -- the committed prefix until the bounded64KiB quantum is reached.
            if OK then Object.Status := Committing; end if;
         when Committing =>
            Object.Status := Failed;
            Storage.Step (Object.Items (Object.Current));
            if Storage.Snapshot (Object.Items (Object.Current)).Phase = Storage.Ready then
               Object.Status := Publishing;
            end if;
         when Publishing =>
            Object.Status := Failed;
            Extend (Object.Current, V.Base, V.Published, OK);
            if OK then Object.Status := Checking; end if;
         when Admitting =>
            Object.Status := Failed;
            Admit (Object.Target, OK);
            if OK then Object.Status := Idle; end if;
         when Idle | Failed => null;
      end case;
      -- Callback success is not renewed authority. In particular, Admit must
      -- not expose Idle/success after revocation during the final callback.
      -- Keep all committed prefixes retained and make this failure sticky.
      if not Owner_Ready then Object.Status := Failed; end if;
   end Step;
end Intel_GPU_Metadata_Bundle;
