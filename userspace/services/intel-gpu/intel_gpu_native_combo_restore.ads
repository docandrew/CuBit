generic
   -- Trusted retained local state, not IPC flags. Caller serializes the whole
   -- boot transition and coordinates firmware; these callbacks do not lock it.
   with function PW1_Held return Boolean;
   with function DC_Disabled return Boolean;
   with function Pages_Ready return Boolean;
package Intel_GPU_Native_Combo_Restore is
   function Execute (Owner : Boolean) return String;
   function Last_Succeeded return Boolean;
   -- Boot-only one attempt. No unmap, power release or automatic recovery.
end Intel_GPU_Native_Combo_Restore;
