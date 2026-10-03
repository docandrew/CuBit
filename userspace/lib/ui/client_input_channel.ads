with Client_Input_Batch_Cache;
package Client_Input_Channel with SPARK_Mode => Off is
   package Cache renames Client_Input_Batch_Cache;
   -- One process-lifetime page shared by serialized fetches. Never spins.
   -- Loaded=False leaves the cache unchanged and permits ordinary polling.
   -- Atomic, read-only sticky state; no IPC or lock acquisition.
   function Is_Disabled return Boolean;
   procedure Fetch
     (S : in out Cache.State; Surface : Cache.W.Identity;
      After : Cache.W.Word; Loaded : out Boolean);
end Client_Input_Channel;
