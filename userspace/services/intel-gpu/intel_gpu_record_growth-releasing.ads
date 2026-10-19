generic
   with procedure Release_Arena (Object : in out Storage.Arena; Accepted : out Boolean);
package Intel_GPU_Record_Growth.Releasing is
   -- Give back the controller's reservation once it is at rest and nothing
   -- uses its storage any more. The controller is then unconfigured.
   procedure Release (Object : in out Controller; Accepted : out Boolean);
end Intel_GPU_Record_Growth.Releasing;
