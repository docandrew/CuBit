--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-kinds (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog.
pragma Ada_2022;

--  The settings of class Secret (setting-catalog.ccl): Config serves them
--  only to a scope that names their key or family (Secret_Scope).
package CCL.Setting_Secrets with SPARK_Mode, Pure is

   function Starts (Key, Prefix : String) return Boolean is
     (Key'Length >= Prefix'Length
      and then Key (Key'First .. Key'First + Prefix'Length - 1) = Prefix);

   --  The secret key or family Key belongs to; empty when it is no secret.
   function Secret_Scope (Key : String) return String is
     (if Key'Length > 12 and then Starts (Key, "wifi.secret.") then "wifi.secret."
      else "");

   function Is_Secret (Key : String) return Boolean is (Secret_Scope (Key)'Length > 0);
end CCL.Setting_Secrets;
