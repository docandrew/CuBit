with Interfaces;
package Intel_GPU_Native_Reset is
   -- Caller has mapped the six approved pages at61200000, forcewake page
   -- at60200000, and readonly BAR at60000000. Static8086:46D2 D0 ownership,
   -- authenticated fuse and exclusive submission ownership are prerequisites.
   -- One shot, retaining power on success/failure; no firmware/PTE publication.
   function Execute (Fuse : Interfaces.Unsigned_32) return String;
   function Last_Succeeded return Boolean;
end Intel_GPU_Native_Reset;
