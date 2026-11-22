--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  The typed setting browser.servo.site-agent.<member>: a CCL Site_User_Agent (CCL.Interfaces.Browser_Agent.SITE_TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CuBit.Canonical_Readers).
package CCL.Setting_Codecs.Site_User_Agent with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type Agent_Presentation_Value is (Penny, Linux, Windows, Mac);
   type Site_User_Agent_Value is record
      presentation : Agent_Presentation_Value := Agent_Presentation_Value'First;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Site_User_Agent. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Site_User_Agent_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Site_User_Agent;

