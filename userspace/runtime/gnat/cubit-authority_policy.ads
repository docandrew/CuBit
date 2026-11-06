--  Pure decision core. Inputs are trusted, already scope-specific evidence;
--  this package neither authenticates evidence nor installs capabilities.
package CuBit.Authority_Policy with Pure, SPARK_Mode is
   type Decision is
     (Approved, Not_Requested, Installation_Denied, Session_Denied,
      Issuer_Denied);
   function Evaluate
     (Requested, Installation_Approved, Session_Approved,
      Issuer_Allowed : Boolean)
      return Decision is
     (if not Requested then Not_Requested
      elsif not Installation_Approved then Installation_Denied
      elsif not Session_Approved then Session_Denied
      elsif not Issuer_Allowed then Issuer_Denied
      else Approved)
     with Post =>
       (Evaluate'Result = Approved) =
         (Requested and Installation_Approved and
          Session_Approved and Issuer_Allowed);

   type Bootstrap_Authority is
     (Log_Publication, Log_Observation, Master_Audio, Clock_Adjustment,
      Metric_Publication, Metric_Observation, Process_Observation, Log_Control,
      --  Whole-disk access for the installer (CuBit.Disk_Install).
      Disk_Installation,
      --  Asking installer.svc to list and install (CuBit.Installer_Protocol).
      Installer_Use,
      --  Applying network settings through netmgr (CuBit.Net_Manager_Control:
      --  per-interface addressing, DNS): the Deskbar's network panel.
      Network_Configuration,
      --  acpi.svc's full-scope endpoint to acpi-backend.svc's named
      --  registers (role 35, ACPI_Register_Roles.Frontend_Tag).
      ACPI_Register_Frontend,
      --  A read-only copy of the composed frame (CuBit.Display_Capture,
      --  role 39): Desktop's endpoint badged for capture.
      Display_Capture);
   --  Authorities the trusted plan must also name in the program's own
   --  entry, (acpi-registers approve-declared), (display-capture
   --  approve-declared): being started by the plan is not enough, and
   --  Bootstrap_Approves alone never grants them.
   subtype Declared_Authority is Bootstrap_Authority
     range ACPI_Register_Frontend .. Display_Capture;

   --  Transitional development-image approval: entry in the trusted startup
   --  plan approves declared privileged requests. OP_SPAWN cannot select it.
   --  Publication preserves the existing manifest-request policy.
   function Bootstrap_Approves
     (Authority : Bootstrap_Authority; Trusted_Startup : Boolean)
      return Boolean is
     (Authority in Log_Publication | Metric_Publication or else
      (Trusted_Startup and then Authority not in Declared_Authority));

   --  The plan's approval of Authority for one of its entries: a declared
   --  authority needs that entry's own approval as well.
   function Startup_Approves
     (Authority : Bootstrap_Authority; Trusted_Startup, Entry_Approves : Boolean)
      return Boolean is
     (if Authority in Declared_Authority then Trusted_Startup and then Entry_Approves
      else Bootstrap_Approves (Authority, Trusted_Startup))
     with Post => (if Authority in Declared_Authority
                   then Startup_Approves'Result = (Trusted_Startup and Entry_Approves));
end CuBit.Authority_Policy;
