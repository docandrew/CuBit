package body Intel_GPU_Domain_Lease is
   function State (Object : Lease) return Ownership_State is (Object.Current);
   function Uncertain (Object : Lease) return Selection is (Object.Unknown);
   procedure Cleanup (Object : in out Lease; Success : out Boolean) is
      OK : Boolean;
   begin
      Success := True;
      for Item in reverse Domain loop
         if Object.Owned (Item) then
            Object.Unknown (Item) := True;
            Release_Domain (Item, OK);
            if OK then
               Object.Owned (Item) := False;
               Object.Unknown (Item) := False;
            else
               Success := False;
            end if;
         end if;
      end loop;
   end Cleanup;
   procedure Acquire (Object : in out Lease; Required : Selection; Success : out Boolean) is
      OK, Clean : Boolean;
   begin
      Success := False;
      if Object.Current /= Idle or else Required = Selection'[others => False] then return; end if;
      Object.Current := Faulted;
      for Item in Domain loop
         if Required (Item) then
            Object.Unknown (Item) := True;
            Acquire_Domain (Item, OK);
            if not OK then
               Cleanup (Object, Clean);
               return;
            end if;
            Object.Owned (Item) := True;
            Object.Unknown (Item) := False;
         end if;
      end loop;
      Object.Current := Held;
      Success := True;
   end Acquire;
   procedure Release (Object : in out Lease; Success : out Boolean) is
   begin
      Success := False;
      if Object.Current /= Held then return; end if;
      Object.Current := Faulted;
      Cleanup (Object, Success);
      if Success then Object.Current := Idle; end if;
   end Release;
end Intel_GPU_Domain_Lease;
