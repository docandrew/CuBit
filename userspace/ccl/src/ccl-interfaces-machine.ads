with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  Turning the machine off from CCL (PWR-003 slice 1): (machine.power-off)
--  and (machine.halt), also the commands (power-off) and (halt). Each asks
--  power.svc once, as the console's requester badge (CuBit.Power_Requests,
--  catalog role power-console, granted only from the startup plan), and
--  answers a typed Power_Answer at once: Accepted (the system stops; the
--  console with it), Busy (a stop already runs), Not_Approved (not a
--  requester's badge), Malformed, Not_Ready (power.svc is not registered),
--  Unavailable (this program has no power endpoint, or power.svc did not
--  answer). The interface is published only where the endpoint is.
--  The types are CCL source; the digest and key are SHA-256 of it (then "#"
--  and the type's name), and tests/ccl-console checks them.
package CCL.Interfaces.Machine with SPARK_Mode is
   use Standard.Interfaces;

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 1;
   PUBLISHED_TYPES : constant := 1;

   TYPE_SOURCE : constant String :=
     "(type Power_Answer (enum Accepted Busy Not_Approved Malformed Not_Ready Unavailable))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#50F1_F7AF_341A_A54A#,
      16#26C1_4501_5079_4703#,
      16#D8D8_3228_EB6E_8BF6#,
      16#71FA_E3EC_D64E_F453#];
   ANSWER_KEY : constant CCL.Objects.Schema_Key :=
     [16#8669_0C07_1CEE_21FF#,
      16#302B_33F7_6408_0C06#,
      16#C299_386C_C05D_CE8D#,
      16#DC37_B06A_42EF_E433#];

   --  Power_Answer's members, in order.
   type Answer is (Accepted, Busy, Not_Approved, Malformed, Not_Ready, Unavailable);

   type Operation is (Power_Off, Halt);
   function Name (Op : Operation) return String is
     (case Op is when Power_Off => "power-off", when Halt => "halt");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Machine);
   function Binding_Of (Op : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Op));

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Contract : out CCL.Objects.Binding; Accepted : out Boolean);

   --  The schema and the interface (machine.power-off, machine.halt, and
   --  the commands power-off and halt).
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  A Power_Answer value.
   procedure Answer_Value
     (Contract : CCL.Objects.Binding; Item : Answer; Result : out CCL.Objects.Image; Built : out Boolean);
end CCL.Interfaces.Machine;
