-- How many render sessions intel-gpu can hold at once, from the hardware.
--
-- The GuC names every context it schedules by a 16-bit ID; 0xFFFF is the
-- invalid ID (i915 intel_guc_fwif.h: GUC_MAX_CONTEXT_ID = 65535, also
-- GUC_INVALID_CONTEXT_ID). The driver gives its own contexts the IDs from
-- First_Context_ID on: the boot render context first, then one per
-- application render session. So the session limit is the rest of the GuC
-- ID space. Nothing else here is a policy number: per-session storage is
-- reserved for this many sessions as virtual address space and committed
-- only as sessions are admitted (Intel_GPU_Slot_Storage), so memory, not a
-- table size, is what runs out first; then admission is refused.
package Intel_GPU_Session_Limits with SPARK_Mode, Pure is
   -- GuC context IDs: 0 .. Invalid_Context_ID - 1.
   Invalid_Context_ID : constant := 16#FFFF#;
   -- The driver's first GuC context ID (the boot render context's).
   First_Context_ID : constant := 7;
   -- Contexts the driver keeps for itself (the boot render context).
   Boot_Contexts : constant := 1;
   -- Every context the driver can name: IDs First_Context_ID .. 0xFFFE.
   Context_Limit : constant := Invalid_Context_ID - First_Context_ID;
   -- One GuC context per application session.
   Session_Limit : constant := Context_Limit - Boot_Contexts;
   -- Sessions whose storage is committed at once when the table must grow:
   -- one commit per store covers this many more sessions.
   Growth_Step : constant := 8;
   pragma Compile_Time_Error
     (Growth_Step > Session_Limit, "growth step exceeds the session limit");
end Intel_GPU_Session_Limits;
