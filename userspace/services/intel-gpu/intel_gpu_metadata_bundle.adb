package body Intel_GPU_Metadata_Bundle is
   use type Storage.State;
   function State (Object : Bundle) return Phase is (Object.Status);
   procedure Request
     (Object : in out Bundle; Count, Record_Quota : Positive;
      Per_Table_Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Status /= Idle or else not Owner_Ready or else
        Count > Record_Quota or else Per_Table_Bytes = 0 or else
        Per_Table_Bytes mod Storage.Page_Bytes /= 0 or else
        (Object.Byte_Limit /= 0 and Object.Byte_Limit /= Per_Table_Bytes)
      then return; end if;
      Object.Target := Count;
      Object.Byte_Limit := Per_Table_Bytes;
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
            if Capacity (Object.Current) >= Object.Target then
               if Object.Current = Table_ID'Last then Object.Status := Admitting;
               else Object.Current := Table_ID'Succ (Object.Current); end if;
            else
               Object.Status := (if V.Phase = Storage.Empty then Opening else Requesting);
            end if;
         when Opening =>
            Object.Status := Failed;
            Storage.Open (Object.Items (Object.Current), Object.Byte_Limit, OK);
            if OK then Object.Status := Requesting; end if;
         when Requesting =>
            Object.Status := Failed;
            if V.Published >= Object.Byte_Limit then return; end if;
            Storage.Request (Object.Items (Object.Current), V.Published +
              Unsigned_64'Min (Storage.Step_Bytes, Object.Byte_Limit - V.Published), OK);
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
   end Step;
end Intel_GPU_Metadata_Bundle;
