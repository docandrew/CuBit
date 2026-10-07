generic
package Intel_GPU_Buffer_Requests.Contexts is
   -- Trusted dispatcher only, after authenticating a newly admitted session.
   -- These tickets identify whole context parents, not application BOs,
   -- replacement tables or pinned bootstrap storage. Zero session is denied.
   -- Reuse requires this child's exact acknowledgment; each new reservation
   -- advances the allocation generation and changes its session owner.
   procedure Reserve
     (Object : in out Service; Session : Unsigned_64; ID : out Ticket;
      Pages : Intel_GPU_Buffer_Backing.Page_Count);
   -- Cleanup authority is independent of live application request authority.
   -- Requires Retire_Session to have closed this exact parent, no pending
   -- allocation, current device ownership, and an unused full ticket identity.
   -- This is metadata eligibility, NOT hardware/reference retirement evidence.
   function Can_Retire
     (Object : Service; Session : Unsigned_64; ID : Ticket) return Boolean;
   -- Trusted cleanup coordinator only, after exact supervisor slot/generation
   -- acknowledgment AND removal of all GPU/CPU/offline VM references. Does not
   -- send IPC, free memory, restore app authority, or recycle a session tag.
   -- False evidence, stale/duplicate tickets and other allocation kinds reject.
   procedure Acknowledge
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean);
end Intel_GPU_Buffer_Requests.Contexts;
