generic
   with procedure Release_Arena (Object : in out Storage.Arena; Accepted : out Boolean);
package Intel_GPU_Metadata_Bundle.Releasing is
   -- Give back every table's reservation (Intel_GPU_Metadata_Arena
   -- Releasing) once the bundle is at rest (Idle or Failed) and nothing uses
   -- its storage any more. The bundle is then empty again.
   procedure Release (Object : in out Bundle; Accepted : out Boolean);
end Intel_GPU_Metadata_Bundle.Releasing;
