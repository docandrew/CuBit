pragma Ada_2022;
with ACPI_Native_PCI_Windows;
package body ACPI_Local_Memory is
   use type Word, ACPI_Backend_Device_Arenas.Outcome;
   function Observe return Report is
      Result : Report := Info;
   begin
      Result.Pool := ACPI_Platform_Pool_Native.Owner.Observe (Pool_State);
      return Result;
   end Observe;
   function Cache_Arena_Is_Exclusive (First, Limit : Word) return Boolean is
     (Info.Stage = Initializing and then Info.Arena_Claimed
      and then Info.Partition.Has_Data
      and then First = Info.Partition.Data.First
      and then Limit = Info.Partition.Data.Limit
      and then Arena_Is_Exclusive (First,Limit));
   function Cache_Access_Allowed (For_Write : Boolean) return Boolean is
     (Info.Stage = Ready and then Field_Access_Allowed (For_Write));
   -- Hardware.Admitting is a BEGIN/SETUP predicate: it correctly becomes
   -- false while the physical lease is held. The same retained Field guard
   -- validates datum access inside that lease; do not reject it here.
   procedure Fail (Why : Reason) is
   begin Info.Stage := Failed; Info.Status := Why; end Fail;
   procedure Initialize (Pool : Pool_Configuration; PCI : PCI_Configuration;
      Accepted : out Boolean) is
      First, Limit : Word := 0;
      Good : Boolean := False;
   begin
      Accepted := False;
      if Info.Stage /= Fresh then return; end if;
      Info.Stage := Initializing;
      if not Hardware.Admitting or else not Setup_Allowed or else Local_Scope = 0 then
         Fail (Owner_Unavailable); return;
      end if;
      if not Pool.Present and then not PCI.Present then
         Info.Stage := Ready; Info.Status := Initialized; Accepted := True; return;
      end if;
      Hardware.Claim_Unused_Arena (First,Limit,Good);
      if not Good then Fail (Arena_Refused); return; end if;
      Info.Arena_Claimed := True;
      if not Arena_Is_Exclusive (First,Limit) then Fail (Arena_Refused); return; end if;
      Info.Partition := ACPI_Backend_Device_Arenas.Plan
        ((First => First,Limit => Limit),PCI.Present,Pool.Present);
      if Info.Partition.Status /= ACPI_Backend_Device_Arenas.Ready
        or else Info.Partition.Has_PCI /= PCI.Present
        or else Info.Partition.Has_Data /= Pool.Present then
         Fail (Invalid_Partition); return;
      end if;
      if Pool.Present then
         if not Setup_Allowed then Fail (Setup_Stopped); return; end if;
         ACPI_Platform_Pool_Native.Acquire (Pool_State,Pool.Endpoint,
           Pool.Expected_Owner,Pool.Request,Pool.Deadline_Milliseconds,Good);
         if not Good then Fail (Pool_Refused); return; end if;
         Info.Pool_Acquired := True;
         if not Setup_Allowed or else not Hardware.Admitting then
            Fail (Setup_Stopped); return;
         end if;
         Regions.Cache.Initialize (Memory,ACPI_Platform_Pool_Native.Owner.Pool (Pool_State),
           Info.Partition.Data.First,Info.Partition.Data.Limit,Good);
         if not Good then Fail (Cache_Refused); return; end if;
         Info.Cache_Ready := True;
      end if;
      if PCI.Present then
         if not Setup_Allowed or else not Hardware.Admitting then
            Fail (Setup_Stopped); return;
         end if;
         ACPI_Native_PCI_Windows.Initialize (PCI.Endpoint,PCI.Expected_Owner,
           Local_Scope,PCI.Epoch,Info.Partition.PCI.First,Info.Partition.PCI.Limit,Good);
         if not Good then Fail (PCI_Refused); return; end if;
         Info.PCI_Ready := True;
      end if;
      Info.Stage := Ready; Info.Status := Initialized; Accepted := True;
   end Initialize;
   procedure Close_Admission is
   begin
      -- No resetting or releasing the retained owner/cache/arena.
      Info.Stage := Closed;
      Regions.Cache.Close_Admission (Memory);
      ACPI_Platform_Pool_Native.Owner.Close_Admission (Pool_State);
   end Close_Admission;
   procedure Retry_Disposal (Completed : out Boolean) is
   begin
      Completed := False;
      if not Setup_Allowed then return; end if;
      ACPI_Platform_Pool_Native.Owner.Retry_Disposal (Pool_State,Completed);
   end Retry_Disposal;
end ACPI_Local_Memory;
