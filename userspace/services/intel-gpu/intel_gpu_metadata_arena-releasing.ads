generic
   -- Give back an exact reservation (Base and the Limit it was opened with).
   with function Unreserve (Base, Bytes : Unsigned_64) return Boolean;
package Intel_GPU_Metadata_Arena.Releasing is
   -- Give back the arena's whole reservation and return it to Empty. Only
   -- when no record anywhere still uses its storage (the owning session was
   -- retired and its records are reset next). An Empty arena accepts.
   -- Accepted = False: the reservation stays (never reused, never lost).
   procedure Release (Object : in out Arena; Accepted : out Boolean);
end Intel_GPU_Metadata_Arena.Releasing;
