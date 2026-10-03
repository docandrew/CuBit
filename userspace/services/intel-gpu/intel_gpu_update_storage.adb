with Intel_GPU_Application_State;
with Intel_GPU_Table_Provenance;
package body Intel_GPU_Update_Storage is
   package S renames Intel_GPU_Application_State;
   package P renames Intel_GPU_Table_Provenance;
   function Table_Metadata_Bytes return Unsigned_64 is
     ((Unsigned_64 (S.Table_Pages) * Unsigned_64 (P.Mapping'Object_Size / 8)
       + 4095) / 4096 * 4096);
   function Mirror_Metadata_Bytes return Unsigned_64 is
     (Unsigned_64 (S.Table_Pages - S.Bootstrap_Table_Mirrors) * 4096);
   use type Storage.State;
   function Pending (Object : Pool) return Boolean is
     (Object.State in Growing | Installing | Attaching | Mirroring);
   function Ready (Object : Pool) return Boolean is
     (Object.State = Idle and then Owner_Ready and then S.Has_Update (Object.Index) and then
      P.Capacity (S.Updates (Object.Index).Table_Owners) >= S.Table_Pages and then
      S.VM.Metadata_Capacity (S.Updates (Object.Index).Candidate) >= S.Table_Pages);
   -- Both the provenance ledger and CPU mirror must exist before cloning.
   procedure Request
     (Object : in out Pool; Index : Positive; Byte_Quota : Unsigned_64;
      Accepted : out Boolean) is
      OK : Boolean;
      V : Storage.View;
      Needed, Image_Bytes, Ledger_Bytes : Unsigned_64;
   begin
      Accepted := False;
      if Object.State /= Idle or else not Owner_Ready or else
        Index > S.Update_Capacity or else Byte_Quota = 0 or else
        Byte_Quota mod Storage.Page_Bytes /= 0 or else
        (Object.Limit /= 0 and Object.Limit /= Byte_Quota) then return; end if;
      Object.Index := Index;
      if S.Has_Update (Index) and then
        P.Capacity (S.Updates (Index).Table_Owners) >= S.Table_Pages and then
        S.VM.Metadata_Capacity (S.Updates (Index).Candidate) >= S.Table_Pages
      then Accepted := True; return; end if;
      V := Storage.Snapshot (Object.Arena);
      if V.Phase = Storage.Empty then
         Object.Limit := Byte_Quota;
         Object.State := Failed;
         Storage.Open (Object.Arena, Byte_Quota, OK);
         if not OK then return; end if;
         V := Storage.Snapshot (Object.Arena);
      end if;
      Object.State := Failed;
      Image_Bytes := (if S.Has_Update (Index) then 0 else S.Update_Storage_Bytes);
      Ledger_Bytes := (if S.Has_Update (Index) and then
        P.Capacity (S.Updates (Index).Table_Owners) >= S.Table_Pages
        then 0 else Table_Metadata_Bytes);
      Needed := Image_Bytes + Ledger_Bytes + Mirror_Metadata_Bytes;
      if V.Published > Byte_Quota or else
        Needed > Byte_Quota - V.Published then return; end if;
      Object.Offset := V.Published;
      Object.Ledger_Offset := Object.Offset + Image_Bytes;
      Object.Mirror_Offset := Object.Ledger_Offset + Ledger_Bytes;
      Object.Mirror_Published := 0;
      Storage.Request (Object.Arena, Object.Offset + Needed, OK);
      if not OK then return; end if;
      Object.State := Growing;
      Accepted := True;
   end Request;
   procedure Step (Object : in out Pool) is
      OK : Boolean;
   begin
      if not Pending (Object) then return; end if;
      if not Owner_Ready then Object.State := Failed; return; end if;
      if Object.State = Growing then
         Storage.Step (Object.Arena);
         case Storage.Snapshot (Object.Arena).Phase is
            when Storage.Ready => Object.State :=
              (if S.Has_Update (Object.Index) then Attaching else Installing);
            when Storage.Growing => null;
            when others => Object.State := Failed;
         end case;
      elsif Object.State = Installing then
         Object.State := Failed;
         S.Install_Fresh_Update (Object.Index,
           Storage.Address (Object.Arena, Object.Offset, S.Update_Storage_Bytes),
           S.Update_Storage_Bytes, OK);
         if OK then Object.State := Attaching; end if;
      elsif Object.State = Attaching then
         Object.State := Failed;
         if not S.Has_Update (Object.Index) then return; end if;
         OK := P.Capacity (S.Updates (Object.Index).Table_Owners) >= S.Table_Pages;
         if not OK then
            P.Extend (S.Updates (Object.Index).Table_Owners,
              Storage.Address (Object.Arena, Object.Ledger_Offset, Table_Metadata_Bytes),
              Table_Metadata_Bytes, OK);
         end if;
         if OK then Object.State := Mirroring; end if;
      else
         Object.State := Failed;
         if not S.Has_Update (Object.Index) then return; end if;
         Object.Mirror_Published := Object.Mirror_Published + Unsigned_64'Min
           (65536, Mirror_Metadata_Bytes - Object.Mirror_Published);
         S.VM.Extend_Metadata (S.Updates (Object.Index).Candidate,
           Storage.Address (Object.Arena, Object.Mirror_Offset, Mirror_Metadata_Bytes),
           Object.Mirror_Published, OK);
         if OK then Object.State :=
           (if Object.Mirror_Published = Mirror_Metadata_Bytes then Idle else Mirroring);
         end if;
      end if;
      if not Owner_Ready then Object.State := Failed; end if;
   end Step;
end Intel_GPU_Update_Storage;
