------------------------------------------------------------------------------
--  CuBit procmgr
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a launched child receives (CAP-012, docs/authority-model.md finding
--  P-03): the intersection of its declared requests, the plan's approval and
--  what its launcher holds, by rights and not by role alone.
--
--  @description
--  A program launched into a session (OP_LAUNCH through a Launch badge) is
--  checked against the session's envelope (Session_Launch), with Decide
--  below and Covers for rights. A program a supervisor starts (Start_Child)
--  is attenuated to the supervisor, as here. For each declared request:
--
--  * the launcher holds it when it holds that kind of authority with every
--    right the request asks: a service role through one endpoint with at
--    least those rights (Read, Write), or as that role's registered
--    provider; an offerable self endpoint; a render approval; a network
--    scope covering the requested one. Nothing else is ever passed on
--    (notifications, framebuffer, firmware, hardware, publications);
--  * a request the launcher holds is given as declared, subject to the
--    plan's approval exactly as for the launcher (startup-only kinds stay
--    refused unless the launcher holds and was approved for them);
--  * an optional request the launcher does not hold is withheld: the child
--    starts without it, and it is never introduced later;
--  * a required request the launcher does not hold refuses the launch;
--  * a filesystem, config, TLS or metrics scope its launcher does not
--    cover (CuBit.Launch_Authority.Scope_Covered) is withheld as well: the
--    child gets the intersection.
--
--  A launcher's endpoints to one role are kept as the set of rights masks
--  it holds (Held_Masks): a request is covered only by one endpoint that
--  has all of its rights, never by two narrower ones together.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Launch_Attenuation with SPARK_Mode, Pure is

   --  A manifest request's rights (.cubit.caps): bit 0 Read, bit 1 Write.
   Read_Right  : constant Unsigned_8 := 1;
   Write_Right : constant Unsigned_8 := 2;
   subtype Rights_Mask is Unsigned_8 range 0 .. Read_Right + Write_Right;

   --  The rights masks of the endpoints a process holds to one role: bit M
   --  set when it holds an endpoint whose rights are exactly M.
   subtype Held_Masks is Unsigned_8;
   None_Held : constant Held_Masks := 0;

   function Covers (Held, Asked : Rights_Mask) return Boolean is
     ((Asked and not Held) = 0);

   --  Holding one more endpoint, with rights Kept.
   function With_Endpoint (Masks : Held_Masks; Kept : Rights_Mask) return Held_Masks is
     (Masks or Shift_Left (Unsigned_8'(1), Natural (Kept)));

   function Holds_Mask (Masks : Held_Masks; M : Rights_Mask) return Boolean is
     ((Masks and Shift_Left (Unsigned_8'(1), Natural (M))) /= 0);

   --  Some one endpoint held covers Asked.
   function Endpoint_Covers (Masks : Held_Masks; Asked : Rights_Mask) return Boolean is
     (for some M in Rights_Mask => Holds_Mask (Masks, M) and then Covers (M, Asked));

   --  The launcher holds a service request for Asked rights: one endpoint
   --  covering it, or it is the role's provider (the issuer of its own
   --  endpoint, with every right).
   function Service_Held
     (Masks : Held_Masks; Is_Provider : Boolean; Asked : Rights_Mask) return Boolean is
     (Is_Provider or else Endpoint_Covers (Masks, Asked));

   type Verdict is (Given, Withheld, Refused);

   --  One declared request of a launched child.
   function Decide (Held, Optional : Boolean) return Verdict is
     (if Held then Given elsif Optional then Withheld else Refused)
   with Post => (Decide'Result = Given) = Held
                and then (Decide'Result = Refused) = (not Held and then not Optional);

   --  No amplification by rights: whatever is given was held with at least
   --  its rights through one endpoint (or by the provider).
   procedure Lemma_No_Amplification
     (Masks : Held_Masks; Is_Provider : Boolean; Asked : Rights_Mask; Optional : Boolean)
   with Ghost, Global => null,
        Pre  => Decide (Service_Held (Masks, Is_Provider, Asked), Optional) = Given
                and then not Is_Provider,
        Post => (for some M in Rights_Mask =>
                   Holds_Mask (Masks, M) and then Covers (M, Asked));

   --  A read-only holder never covers a request for Write.
   procedure Lemma_Read_Only
     (Asked : Rights_Mask)
   with Ghost, Global => null,
        Pre  => (Asked and Write_Right) /= 0,
        Post => not Endpoint_Covers (With_Endpoint (None_Held, Read_Right), Asked);

end Launch_Attenuation;
