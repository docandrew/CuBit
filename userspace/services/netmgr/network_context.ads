------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The hook for CTX-001's network context (docs/security-model.md,
--  docs/net-link.md section 7): netmgr reports each interface's link
--  transition here as a typed record. It does nothing yet: the network
--  context design (CTX-001) decides what is published, to whom, and what
--  else a context needs (the ESS identity and trust class of a Wi-Fi
--  network, docs/wifi.md "Network security contexts").
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Link;

package Network_Context with SPARK_Mode is
   package NL renames CuBit.Net_Link;

   type Link_Transition is record
      Name      : NL.Interface_Name;
      Handle    : NL.Interface_Handle;
      Condition : NL.Link_Condition := NL.Down;
   end record;

   procedure Changed (Transition : Link_Transition) is null;
end Network_Context;
