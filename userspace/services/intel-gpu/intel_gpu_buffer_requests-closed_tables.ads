generic
package Intel_GPU_Buffer_Requests.Closed_Tables is
   -- Trusted cleanup of replacement tables after session revocation. Closing
   -- preserves the allocation kind, not application authority or permission to
   -- reuse. Never accepts context parents, pinned bootstrap or application BOs.
   function Can_Retire
     (Object : Service; Session : Unsigned_64; ID : Ticket) return Boolean;
   -- Requires exact supervisor acknowledgement and disposal of all references,
   -- including the logical current snapshot if it adopted this table backing.
   -- The dispatcher supplies that evidence; this child does not drain hardware,
   -- authenticate a supervisor reply, free memory or recycle a session tag.
   procedure Acknowledge
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean);
end Intel_GPU_Buffer_Requests.Closed_Tables;
