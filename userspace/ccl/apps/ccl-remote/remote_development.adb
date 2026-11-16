pragma Ada_2022;
with CuBit.Handles.Startup;
with CuBit.Startup_Tables;

package body Remote_Development is

   --  The manifest's DEVELOPMENT-ONLY requests, by startup-table label
   --  (manifest.ccl lists which app needs each), and the outlet's entry.
   type Label is access constant String;
   L01 : aliased constant String := "log-control";
   L02 : aliased constant String := "lexicon";
   L03 : aliased constant String := "installer";
   L04 : aliased constant String := "wifi";
   L05 : aliased constant String := "power";
   L06 : aliased constant String := "llm";
   L07 : aliased constant String := "secrets";
   L08 : aliased constant String := "device-manager";
   L09 : aliased constant String := "gpu-probe";
   L10 : aliased constant String := "mixer";
   L11 : aliased constant String := "stdmetric";
   Services : constant array (1 .. 11) of Label :=
     [L01'Access, L02'Access, L03'Access, L04'Access, L05'Access, L06'Access, L07'Access, L08'Access,
      L09'Access, L10'Access, L11'Access];

   --  The manifest's DEVELOPMENT-ONLY scopes, as written there.
   Scopes : constant String :=
     "@nvme:0/ (read write create), @mem:0/ (read), @cd:0/ (read), " &
     "@ufs:0/Assets/cubit-bible/ (read), @system/Recovery/com.cubit.text/ (read write create), " &
     "config-all (read write)";

   function Granted (Name : String) return Boolean is
     (CuBit.Handles.Startup.Optional_Endpoint_Named (Name, CuBit.Startup_Tables.Endpoint).Present);

   function Summary return String is
      function From (I : Positive) return String is
        (if I > Services'Last then ""
         else (if I > Services'First then ", " else "") & Services (I).all &
              (if Granted (Services (I).all) then "" else " (absent)") & From (I + 1));
   begin
      return "development-only authority for app replace: services " & From (Services'First) &
        "; outlet com.cubit-os.stdmetric; scopes " & Scopes;
   end Summary;

end Remote_Development;
