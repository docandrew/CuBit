with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;

--  The secret store (secrets.svc v0, SEC-022), by name only: a Secret is a
--  reference, never a value, and no operation returns one.
--    (secrets.set (Secret "anthropic-api-key"))
--                       the console asks for the value: the next line typed
--                       is not echoed, not kept in history, and goes only
--                       to secrets.svc. Answers the reference.
--    (secrets.forget s) wipes the stored value.
--    (secrets.status s) Secret_State: whether a value is stored and which
--                       service holds the grant to use it.
--    (secrets.revoke-use s)
--                       withdraws that grant until the next boot.
--  The types are CCL source; the digest and keys are SHA-256 of it (then
--  "#" and the type's name), and tests/ccl-console checks them.
package CCL.Interfaces.Secrets with SPARK_Mode is

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 2;
   PUBLISHED_TYPES : constant := 2;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Secret (record (name String))) (type Secret_State (record (name String) " &
     "(stored Boolean) (holder String)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#1F1D_74F4_A021_6809#,
      16#4A7A_3E66_E2CF_8CE7#,
      16#AA3A_C8A0_19F9_CFF7#,
      16#B945_ECA6_67DF_03A3#];
   SECRET_KEY : constant CCL.Objects.Schema_Key :=
     [16#6888_A0D5_EDEA_10C3#,
      16#BCBF_FAB4_DC75_F97E#,
      16#4A0C_7702_5BC9_DDCB#,
      16#8E04_6BEB_F4A6_4836#];
   STATE_KEY : constant CCL.Objects.Schema_Key :=
     [16#6DCC_08B1_497E_E6A4#,
      16#2BE0_30A9_B488_7711#,
      16#F67D_76C9_A749_3480#,
      16#FE04_4766_6CC5_BDF2#];

   SECRET_NAME : constant := 1;
   STATE_FIELDS : constant := 3;

   type Operation is (Set, Forget, Status, Revoke_Use);
   function Name (Item : Operation) return String is
     (case Item is
         when Set => "set", when Forget => "forget", when Status => "status",
         when Revoke_Use => "revoke-use");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Secrets);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Secret, State : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Secrets;
