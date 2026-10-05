with Intel_GPU_Application_State;
with Intel_GPU_Table_Provenance;
package body Intel_GPU_Update_Storage is
   package S renames Intel_GPU_Application_State;
   package P renames Intel_GPU_Table_Provenance;
   use type Storage.State, Records.Phase, Records.Element_Access;
   use type S.Installation_Phase;
   function Table_Metadata_Bytes return Unsigned_64 is
     ((Unsigned_64 (S.Table_Pages) * Unsigned_64 (P.Mapping'Object_Size / 8)
       + 4095) / 4096 * 4096);
   function Mirror_Metadata_Bytes return Unsigned_64 is
     (Unsigned_64 (S.Table_Pages - S.Bootstrap_Table_Mirrors) * 4096);
   function Charged_Bytes (Object : Pool) return Unsigned_64 is (Object.Account.Used);
   function Charge (Account : in out Budget; Bytes : Unsigned_64) return Boolean is
   begin
      if Account.Used > Account.Limit or else Bytes = 0 or else
        Bytes > Account.Limit - Account.Used then return False; end if;
      Account.Used := Account.Used + Bytes;
      return True;
   end Charge;
   function Pending (Object : Pool) return Boolean is
     (Object.State not in Idle | Failed);
   function Capacity (Object : Pool; Kind : Region_Kind) return Natural is
     (if not S.Has_Update (Object.Index) then 0 else
      (case Kind is
       when Image => 1,
       when Ledger => P.Capacity (S.Updates (Object.Index).Table_Owners),
       when References => S.Table_References.Capacity (S.Updates (Object.Index).Table_IDs),
       when Descriptors => S.VM.Descriptor_Capacity (S.Updates (Object.Index).Candidate),
       when Mirrors => S.VM.Metadata_Capacity (S.Updates (Object.Index).Candidate)));
   function Goal (Object : Pool; Kind : Region_Kind) return Positive is
     (if Kind = Image then 1 else Object.Target);
   function Epoch_Ready (Object : Pool) return Boolean is
     (not S.Has_Update (Object.Index) or else
      (S.Updates (Object.Index).Table_Generation = Object.Generation and then
       S.Table_References.Generation (S.Updates (Object.Index).Table_IDs) = Object.Generation));
   function Ready (Object : Pool) return Boolean is
     (Object.Requested and then Object.State = Idle and then Owner_Ready and then Epoch_Ready (Object) and then
      (for all K in Region_Kind => Capacity (Object, K) >= Goal (Object, K)));
   function Limit (Kind : Region_Kind) return Unsigned_64 is
     (case Kind is
      when Image => S.Update_Storage_Bytes,
      when Ledger => Table_Metadata_Bytes,
      when References => S.Table_References.Metadata_Bytes,
      when Descriptors => S.VM.Descriptor_Metadata_Bytes,
      when Mirrors => Mirror_Metadata_Bytes);
   procedure Request
     (Object : in out Pool; Index : Positive; Byte_Quota : Unsigned_64;
      Accepted : out Boolean; Tables : Positive := Intel_GPU_Application_State.Table_Pages) is
      OK : Boolean;
   begin
      Accepted := False;
      if Object.State /= Idle or else not Owner_Ready or else
        Index > S.Update_Capacity or else Tables > S.Table_Pages or else
        Byte_Quota = 0 or else Byte_Quota mod Storage.Page_Bytes /= 0 or else
        (Object.Account.Limit /= 0 and then Object.Account.Limit /= Byte_Quota)
      then return; end if;
      if S.Has_Update (Index) and then
        S.Table_References.Generation (S.Updates (Index).Table_IDs) /=
          S.Updates (Index).Table_Generation then return; end if;
      Object.Index := Index; Object.Target := Tables;
      Object.Generation := (if S.Has_Update (Index) then
         S.Updates (Index).Table_Generation else 1);
      Object.Account.Limit := Byte_Quota;
      Object.Requested := True;
      if Ready (Object) then Accepted := True; return; end if;
      Object.State := Failed;
      Records.Request (Object.Images, Object.Account'Access, Index,
                       Byte_Quota, Byte_Quota, OK);
      if not OK then return; end if;
      Object.State := Registry;
      Accepted := True;
   end Request;
   procedure Step (Object : in out Pool) is
      OK : Boolean;
      V, Other : Storage.View;
      Bytes : Unsigned_64;
   begin
      if not Pending (Object) then return; end if;
      if not Owner_Ready or else not Epoch_Ready (Object) then
         Object.State := Failed; return;
      end if;
      if Object.State = Registry then
         Records.Step (Object.Images, Object.Account'Access);
         case Records.State (Object.Images) is
            when Records.Idle =>
               Object.Current := Records.Lookup (Object.Images, Object.Index);
               Object.Kind := Image;
               Object.State := (if Object.Current = null then Failed else Checking);
            when Records.Failed => Object.State := Failed;
            when others => null;
         end case;
      elsif Object.Current = null then
         Object.State := Failed;
      else
         V := Storage.Snapshot (Object.Current.Items (Object.Kind));
         case Object.State is
            when Checking =>
               if Capacity (Object, Object.Kind) >= Goal (Object, Object.Kind) then
                  if Object.Kind = Region_Kind'Last then Object.State := Idle;
                  else Object.Kind := Region_Kind'Succ (Object.Kind); end if;
               else
                  Object.State := (if V.Phase = Storage.Empty then Opening else Requesting);
               end if;
            when Opening =>
               Object.State := Failed;
               Storage.Open (Object.Current.Items (Object.Kind), Limit (Object.Kind), OK);
               if not OK then return; end if;
               V := Storage.Snapshot (Object.Current.Items (Object.Kind));
               -- Fixed five-entry check, not a walk over the image namespace.
               for K in Region_Kind loop
                  if K /= Object.Kind then
                     Other := Storage.Snapshot (Object.Current.Items (K));
                     if Other.Base /= 0 and then
                       (if V.Base <= Other.Base then V.Limit > Other.Base - V.Base
                        else Other.Limit > V.Base - Other.Base)
                     then return; end if;
                  end if;
               end loop;
               Object.State := Requesting;
            when Requesting =>
               Object.State := Failed;
               if V.Published >= Limit (Object.Kind) then return; end if;
               Bytes := Unsigned_64'Min
                 (Unsigned_64'Min (Storage.Step_Bytes,
                    Unsigned_64'Max (Storage.Page_Bytes, V.Published)),
                  Limit (Object.Kind) - V.Published);
               if not Charge (Object.Account, Bytes) or else not Owner_Ready then return; end if;
               Storage.Request (Object.Current.Items (Object.Kind), V.Published + Bytes, OK);
               if OK then Object.State := Committing; end if;
            when Committing =>
               Object.State := Failed;
               Storage.Step (Object.Current.Items (Object.Kind));
               if Storage.Snapshot (Object.Current.Items (Object.Kind)).Phase = Storage.Ready then
                  Object.State := Publishing;
               end if;
            when Publishing =>
               Object.State := Failed;
               case Object.Kind is
                  when Image =>
                     if V.Published < S.Update_Storage_Bytes then
                        Object.State := Requesting; return;
                     end if;
                     S.Begin_Fresh_Update (Object.Current.Fresh,
                       Object.Index, V.Base, V.Published, OK);
                     if OK then Object.State := Installing; end if;
                     return;
                  when Ledger =>
                     P.Extend (S.Updates (Object.Index).Table_Owners, V.Base, V.Published, OK);
                  when References =>
                     S.Table_References.Extend
                       (S.Updates (Object.Index).Table_IDs, V.Base, V.Published, OK);
                  when Descriptors =>
                     S.VM.Extend_Descriptors
                       (S.Updates (Object.Index).Candidate, V.Base, V.Published, OK);
                  when Mirrors =>
                     S.VM.Extend_Metadata
                       (S.Updates (Object.Index).Candidate, V.Base, V.Published, OK);
               end case;
               if OK then Object.State := Checking; end if;
            when Installing =>
               S.Step_Fresh_Update (Object.Current.Fresh);
               case S.State (Object.Current.Fresh) is
                  when S.Complete => Object.State := Checking;
                  when S.Checking | S.Publishing => null;
                  when others => Object.State := Failed;
               end case;
            when others => Object.State := Failed;
         end case;
      end if;
      if not Owner_Ready or else not Epoch_Ready (Object) then Object.State := Failed; end if;
   end Step;
end Intel_GPU_Update_Storage;
