package body Intel_GPU_Record_Growth is
   function Snapshot (Object : Controller) return View is (Object.Data);

   procedure Configure
     (Object : in out Controller; Byte_Quota : Unsigned_64;
      Record_Quota : Positive; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Data.State /= Empty or else Byte_Quota = 0 or else
        Byte_Quota mod Storage.Page_Bytes /= 0 or else
        Record_Quota < Capacity then return; end if;
      Object.Byte_Limit := Byte_Quota;
      Object.Record_Limit := Record_Quota;
      Object.Data.State := Idle;
      Accepted := True;
   end Configure;

   procedure Request
     (Object : in out Controller; Records : Positive; Accepted : out Boolean) is
      use type Storage.State;
   begin
      Accepted := False;
      if Object.Data.State /= Idle or else Records > Object.Record_Limit then
         return;
      end if;
      -- Known exhaustion is not an ambiguous allocation failure. Reject
      -- before changing Target/State so existing records remain reusable.
      if Records > Capacity and then
        Object.Data.Published_Bytes >= Object.Byte_Limit
      then return; end if;
      Object.Data.Target := Records;
      if Records > Capacity then
         Object.Data.State :=
           (if Storage.Snapshot (Object.Arena).Phase = Storage.Empty
            then Opening else Requesting);
      end if;
      Accepted := True;
   end Request;

   procedure Step (Object : in out Controller) is
      use type Storage.State;
      OK : Boolean;
      Bytes : Unsigned_64;
      Current : Storage.View;
   begin
      case Object.Data.State is
         when Opening =>
            Object.Data.State := Failed;
            Object.Data.Error := Reservation_Failed;
            Storage.Open (Object.Arena, Object.Byte_Limit, OK);
            if OK then
               Object.Data.Error := None;
               Object.Data.State := Requesting;
            end if;
         when Requesting =>
            Current := Storage.Snapshot (Object.Arena);
            if Current.Published >= Object.Byte_Limit then
               Object.Data.State := Failed;
               Object.Data.Error := Byte_Quota_Exhausted;
               return;
            end if;
            Bytes := Current.Published + Unsigned_64'Min
              (Storage.Step_Bytes, Object.Byte_Limit - Current.Published);
            Object.Data.State := Failed;
            Object.Data.Error := Commit_Failed;
            Storage.Request (Object.Arena, Bytes, OK);
            if OK then
               Object.Data.Error := None;
               Object.Data.State := Committing;
            end if;
         when Committing =>
            Object.Data.State := Failed;
            Object.Data.Error := Commit_Failed;
            Storage.Step (Object.Arena);
            if Storage.Snapshot (Object.Arena).Phase = Storage.Ready then
               Object.Data.Error := None;
               Object.Data.State := Publishing;
            end if;
         when Publishing =>
            Current := Storage.Snapshot (Object.Arena);
            Object.Data.State := Failed;
            Object.Data.Error := Publication_Failed;
            Publish (Current.Base, Current.Published, OK);
            if OK then
               Object.Data.Published_Bytes := Current.Published;
               Object.Data.Error := None;
               Object.Data.State :=
                 (if Capacity >= Object.Data.Target then Idle else Requesting);
            end if;
         when Empty | Idle | Failed => null;
      end case;
   end Step;
end Intel_GPU_Record_Growth;
