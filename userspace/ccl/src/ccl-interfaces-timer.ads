with CCL.Catalog;

--  Session timers (docs/ccl-streams.md): the first stream source.
--  (timer.every n) is a Stream<Integer> that ticks every n milliseconds,
--  each element the tick's monotonic time. Its stream lives in the
--  session's table, so it ends with the session.
package CCL.Interfaces.Timer with
   SPARK_Mode => On
is
   --  SHA-256 of interfaces/timer.ccl-interface.
   DESCRIPTOR_DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#5EEA_0076_E8D8_F124#,
      16#B5FB_6C60_14DC_4E70#,
      16#8815_232B_B23C_F7CF#,
      16#9B6C_158D_22BD_5704#];

   procedure Publish
     (Item  : in out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error);

   procedure Resolve_Every
     (Item   : CCL.Catalog.Interface_Catalog;
      Result : out CCL.Catalog.Resolved_Operation;
      Found  : out Boolean);
end CCL.Interfaces.Timer;
