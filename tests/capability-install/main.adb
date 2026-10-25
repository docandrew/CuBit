-- Hosted tests for Capability_Install (CAP-002): every refusal, every kind
-- of narrowing, CSPACE and reply admission, placement into a table (never
-- over a live entry; refusals change nothing), and a randomized
-- differential test of Derive against Reference.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capability_ABI;
with Capability_Install; use Capability_Install;
with Fake_Pages;
with Reference;
with Test_Install; use Test_Install;

procedure Main is
   use type Small.Place_Status;
   use type Small_Tables.Insert_Status;
   use type Small_Tables.Slot_Base;
   use type Fake_Pages.Page_Total;
   package ABI renames Capability_ABI;
   use type ABI.Refusal;

   Checks   : Natural := 0;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   GRANT_ONLY : constant CapabilityRights :=
     (RIGHT_GRANT => True, others => False);
   RW_GRANT : constant CapabilityRights :=
     (RIGHT_READ => True, RIGHT_WRITE => True, RIGHT_GRANT => True,
      others => False);

   function Cap (Kind : CapabilityType; Rights : CapabilityRights;
                 Ref, Param : Unsigned_64; Tag : Unsigned_64 := 0;
                 Gen : Generation := 7) return Capability is
     ((capType => Kind, rights => Rights, authorityTag => Tag,
       object => (ref => Ref, param => Param), gen => Gen));

   function Req (Rights : CapabilityRights; Set_Tag : Boolean := False;
                 Tag : Unsigned_64 := 0; Narrow : Boolean := False;
                 Base, Length : Unsigned_64 := 0) return Request is
     ((Rights => Rights, Set_Tag => Set_Tag, Tag => Tag, Narrow => Narrow,
       Base => Base, Length => Length));

   procedure Expect_Refusal
     (Source : Capability; Item : Request; Why : ABI.Refusal; What : String)
   is
      O : constant Outcome := Derive (Source, Item);
   begin
      Check (not O.Accepted and then O.Why = Why, What);
      Check (not Reference.Derivable (Source, Item), What & " (reference)");
   end Expect_Refusal;

   procedure Expect_Copy
     (Source : Capability; Item : Request; Copy : Capability; What : String)
   is
      O : constant Outcome := Derive (Source, Item);
   begin
      Check (O.Accepted and then O.Copy = Copy, What);
      Check (Reference.Derivable (Source, Item), What & " (reference)");
   end Expect_Copy;

   ---------------------------------------------------------------------------
   procedure Test_Derive is
      Endpoint : constant Capability := Cap (CAP_ENDPOINT, ALL_RIGHTS, 5, 0, 0);
      Tagged_Endpoint : constant Capability :=
        Cap (CAP_ENDPOINT, ALL_RIGHTS, 5, 0, 16#4947#);
      Memory : constant Capability :=
        Cap (CAP_DEVICE_MEM, RW_GRANT, 16#F000_0000#, 16#10_0000#);
      Ports : constant Capability := Cap (CAP_IOPORT, RW_GRANT, 0, 16#1_0000#);
      Legacy : constant Capability := Cap (CAP_IRQ, (RIGHT_READ | RIGHT_GRANT => True,
                                                     others => False), 32, 16);
      Vectors : constant Capability := Cap (CAP_IRQ, (RIGHT_READ | RIGHT_GRANT => True,
                                                      others => False), 48, 79);
      Roles : constant Capability := Cap (CAP_NOTIFICATION, RW_GRANT, 1, 64);
      Share : constant Capability := Cap (CAP_SCHEDULING, GRANT_ONLY, 2_500, 5_000);
      Process : constant Capability := Cap (CAP_PROCESS, ALL_RIGHTS, 0, 0);
   begin
      -- Rights.
      Expect_Copy (Endpoint, Req (READ_WRITE),
                   Cap (CAP_ENDPOINT, READ_WRITE, 5, 0, 0), "endpoint narrowed rights");
      Expect_Copy (Process, Req (ALL_RIGHTS), Process, "equal rights");
      Expect_Refusal (Cap (CAP_ENDPOINT, READ_WRITE, 5, 0), Req (READ_ONLY),
                      ABI.Insufficient_Rights, "source without grant");
      Expect_Refusal (Cap (CAP_ENDPOINT, (RIGHT_READ | RIGHT_GRANT => True, others => False), 5, 0),
                      Req (READ_WRITE), ABI.Would_Amplify, "more rights than source");
      -- Kinds never copied.
      for K in CapabilityType loop
         if K not in CAP_ENDPOINT | CAP_NOTIFICATION | CAP_IOPORT | CAP_IRQ |
                     CAP_PROCESS | CAP_DEVICE_MEM | CAP_SCHEDULING |
                     CAP_FIRMWARE_SNAPSHOT
         then
            Expect_Refusal (Cap (K, ALL_RIGHTS, 5, 1), Req (READ_ONLY),
                            ABI.Not_Transferable, "kind " & K'Image);
         end if;
      end loop;
      -- The firmware snapshot reader: devmgr to procmgr (with Grant), then a
      -- read-only copy to the program; never a tag, never narrowed, never
      -- more than read.
      declare
         Snapshot : constant Capability :=
           Cap (CAP_FIRMWARE_SNAPSHOT, (RIGHT_READ | RIGHT_GRANT => True, others => False), 0, 0);
      begin
         Expect_Copy (Snapshot, Req (READ_ONLY), Cap (CAP_FIRMWARE_SNAPSHOT, READ_ONLY, 0, 0, 0),
                      "firmware snapshot read-only copy");
         Expect_Refusal (Snapshot, Req (READ_WRITE), ABI.Would_Amplify, "firmware snapshot write");
         Expect_Refusal (Cap (CAP_FIRMWARE_SNAPSHOT, READ_ONLY, 0, 0), Req (READ_ONLY),
                         ABI.Insufficient_Rights, "firmware snapshot copy without grant");
      end;
      -- Device vectors: never; legacy lines: yes, and narrowed.
      Expect_Refusal (Vectors, Req (READ_ONLY), ABI.Not_Transferable, "vector authority");
      Expect_Refusal (Cap (CAP_IRQ, GRANT_ONLY, 40, 9), Req (NO_RIGHTS),
                      ABI.Not_Transferable, "range touching the first device vector");
      Expect_Refusal (Cap (CAP_IRQ, GRANT_ONLY, 126, 1), Req (NO_RIGHTS),
                      ABI.Not_Transferable, "the last device vector");
      Expect_Refusal (Cap (CAP_IRQ, GRANT_ONLY, 33, 0), Req (NO_RIGHTS),
                      ABI.Not_Transferable, "an empty interrupt range");
      Expect_Copy (Legacy, Req (READ_ONLY, Narrow => True, Base => 46, Length => 1),
                   Cap (CAP_IRQ, READ_ONLY, 46, 1), "one legacy line");
      Expect_Refusal (Legacy, Req (READ_ONLY, Narrow => True, Base => 46, Length => 3),
                      ABI.Would_Amplify, "legacy lines past the root");
      Expect_Copy (Cap (CAP_IRQ, GRANT_ONLY, 127, 1), Req (NO_RIGHTS),
                   Cap (CAP_IRQ, NO_RIGHTS, 127, 1), "a vector above the device range");
      -- Device memory.
      Expect_Copy (Memory, Req (READ_ONLY, Narrow => True, Base => 16#F000_1000#,
                                Length => 16#1000#),
                   Cap (CAP_DEVICE_MEM, READ_ONLY, 16#F000_1000#, 16#1000#),
                   "device-memory subrange");
      Expect_Copy (Memory, Req (RW_GRANT, Narrow => True, Base => 16#F000_0000#,
                                Length => 16#10_0000#), Memory, "the whole range, narrowed to itself");
      Expect_Refusal (Memory, Req (READ_ONLY, Narrow => True, Base => 16#EFFF_F000#,
                                   Length => 16#2000#), ABI.Would_Amplify, "below the range");
      Expect_Refusal (Memory, Req (READ_ONLY, Narrow => True, Base => 16#F00F_F000#,
                                   Length => 16#2000#), ABI.Would_Amplify, "past the range");
      Expect_Refusal (Memory, Req (READ_ONLY, Narrow => True, Base => 16#F000_0000#,
                                   Length => 0), ABI.Would_Amplify, "an empty range");
      Expect_Refusal (Memory, Req (READ_ONLY, Narrow => True, Base => Unsigned_64'Last,
                                   Length => 2), ABI.Would_Amplify, "a wrapping range");
      -- Ports and roles.
      Expect_Copy (Ports, Req (READ_WRITE, Narrow => True, Base => 16#1F0#, Length => 8),
                   Cap (CAP_IOPORT, READ_WRITE, 16#1F0#, 8), "port subrange");
      Expect_Copy (Roles, Req (READ_ONLY, Narrow => True, Base => 6, Length => 1),
                   Cap (CAP_NOTIFICATION, READ_ONLY, 6, 1), "one notification role");
      Expect_Refusal (Roles, Req (READ_ONLY, Narrow => True, Base => 0, Length => 1),
                      ABI.Would_Amplify, "role outside the root");
      Expect_Copy (Roles, Req (READ_ONLY, Set_Tag => True, Tag => 16#494E#, Narrow => True,
                               Base => 1, Length => 1),
                   Cap (CAP_NOTIFICATION, READ_ONLY, 1, 1, 16#494E#),
                   "one role, badged (input publication)");
      Expect_Refusal (Cap (CAP_NOTIFICATION, RW_GRANT, 1, 64, 16#494E#),
                      Req (READ_ONLY, Set_Tag => True, Tag => 99, Narrow => True,
                           Base => 1, Length => 1),
                      ABI.Would_Amplify, "rebadging a badged notification");
      -- Scheduling shares.
      Expect_Copy (Share, Req (NO_RIGHTS, Narrow => True, Base => 1_500, Length => 5_000),
                   Cap (CAP_SCHEDULING, NO_RIGHTS, 1_500, 5_000), "smaller share");
      Expect_Copy (Share, Req (NO_RIGHTS, Narrow => True, Base => 500, Length => 1_000),
                   Cap (CAP_SCHEDULING, NO_RIGHTS, 500, 1_000), "equal share, other period");
      Expect_Refusal (Share, Req (NO_RIGHTS, Narrow => True, Base => 600, Length => 1_000),
                      ABI.Would_Amplify, "larger share");
      Expect_Refusal (Share, Req (NO_RIGHTS, Narrow => True, Base => 1, Length => 0),
                      ABI.Would_Amplify, "zero period");
      -- Narrowing is for range kinds and shares only.
      Expect_Refusal (Endpoint, Req (READ_ONLY, Narrow => True, Base => 5, Length => 1),
                      ABI.Invalid_Argument, "narrowing an endpoint");
      Expect_Refusal (Process, Req (READ_ONLY, Narrow => True, Base => 0, Length => 1),
                      ABI.Invalid_Argument, "narrowing a process");
      -- Tags.
      Expect_Copy (Endpoint, Req (READ_WRITE, Set_Tag => True, Tag => 99),
                   Cap (CAP_ENDPOINT, READ_WRITE, 5, 0, 99), "badging an untagged endpoint");
      Expect_Copy (Tagged_Endpoint, Req (READ_WRITE),
                   Cap (CAP_ENDPOINT, READ_WRITE, 5, 0, 16#4947#), "a tagged copy keeps its tag");
      Expect_Refusal (Tagged_Endpoint, Req (READ_WRITE, Set_Tag => True, Tag => 99),
                      ABI.Would_Amplify, "retagging a tagged endpoint");
      Expect_Refusal (Endpoint, Req (READ_WRITE, Set_Tag => True, Tag => 0),
                      ABI.Would_Amplify, "a zero badge");
      Expect_Refusal (Ports, Req (READ_WRITE, Set_Tag => True, Tag => 1),
                      ABI.Invalid_Argument, "tagging ports");
      -- Generation and object are kept.
      declare
         O : constant Outcome := Derive (Cap (CAP_PROCESS, ALL_RIGHTS, 9, 0, 0, 1234),
                                         Req (READ_ONLY));
      begin
         Check (O.Accepted and then O.Copy.gen = 1234 and then O.Copy.object.ref = 9,
                "generation and object kept");
      end;
   end Test_Derive;

   ---------------------------------------------------------------------------
   procedure Test_Decode is
      W : ABI.Derivation := [others => 0];
   begin
      W (ABI.Rights) := ABI.Right_Read or ABI.Right_Grant;
      Check (Well_Formed (W), "plain derivation words");
      Check (Decode (W).Rights = CapabilityRights'(RIGHT_READ | RIGHT_GRANT => True, others => False),
             "rights decoded");
      W (ABI.Rights) := 32;
      Check (not Well_Formed (W), "unknown right bit");
      W (ABI.Rights) := 0;
      W (ABI.Flags) := 4;
      Check (not Well_Formed (W), "unknown flag bit");
      W (ABI.Flags) := ABI.Set_Tag;
      Check (not Well_Formed (W), "set tag without a tag");
      W (ABI.Tag) := 5;
      Check (Well_Formed (W) and then Decode (W).Set_Tag and then Decode (W).Tag = 5,
             "set tag");
      W (ABI.Flags) := 0;
      Check (not Well_Formed (W), "a tag without set tag");
      W (ABI.Tag) := 0;
      W (ABI.Base) := 1;
      Check (not Well_Formed (W), "a range without narrow");
      W (ABI.Flags) := ABI.Narrow;
      W (ABI.Length) := 4;
      Check (Well_Formed (W) and then Decode (W).Narrow
             and then Decode (W).Base = 1 and then Decode (W).Length = 4, "narrow");
      for M in Unsigned_64 range 0 .. 31 loop
         Check (Mask_Of (Rights_Of (M)) = M, "rights mask round trip");
      end loop;
   end Test_Decode;

   ---------------------------------------------------------------------------
   procedure Test_Admission is
      Cspace : constant Capability := Cap (CAP_CSPACE, GRANT_ONLY, 12, 0, 0, 3);
      A : Admission;
   begin
      A := Cspace_Admits (Cspace, 12, 3, False);
      Check (A.Admitted, "cspace admits its unstarted child");
      A := Cspace_Admits (Cspace, 12, 3, True);
      Check (not A.Admitted and then A.Why = ABI.Sealed, "sealed once started");
      A := Cspace_Admits (Cspace, 12, 4, False);
      Check (not A.Admitted and then A.Why = ABI.Target_Gone, "stale generation");
      A := Cspace_Admits (Cspace, 13, 3, False);
      Check (not A.Admitted and then A.Why = ABI.Target_Gone, "another process");
      A := Cspace_Admits (Cap (CAP_CSPACE, READ_ONLY, 12, 0, 0, 3), 12, 3, False);
      Check (not A.Admitted and then A.Why = ABI.Insufficient_Rights, "cspace without grant");
      A := Cspace_Admits (Cap (CAP_PROCESS, ALL_RIGHTS, 12, 0, 0, 3), 12, 3, False);
      Check (not A.Admitted and then A.Why = ABI.Wrong_Kind, "not a cspace");
      A := Reply_Admits (Cap (CAP_REPLY, READ_WRITE, 1, 0));
      Check (not A.Admitted and then A.Why = ABI.Not_Accepted, "reply not accepting");
      A := Reply_Admits (Cap (CAP_REPLY, RW_GRANT, 1, 0));
      Check (A.Admitted, "reply accepting");
      A := Reply_Admits (Cap (CAP_ENDPOINT, RW_GRANT, 1, 0));
      Check (not A.Admitted and then A.Why = ABI.Wrong_Kind, "not a reply");
   end Test_Admission;

   ---------------------------------------------------------------------------
   procedure Test_Placement is
      use type Small_Tables.Handle_Check;
      T : Small_Tables.Table;
      Inserted : Small_Tables.Insert_Result;
      Placed : Small.Place_Result;
      Held : array (1 .. 10) of Small_Tables.Handle;
      Source : constant Capability := Cap (CAP_ENDPOINT, ALL_RIGHTS, 5, 0, 0);
      Lookup : Small_Tables.Lookup_Result;
      Before : Small_Tables.Slot_Count;
      Admit_Yes : constant Admission := (Admitted => True);
   begin
      Fake_Pages.Reset;
      Small_Tables.Configure (T, 1, (Kind => Small_Tables.Bounded, Most => 11));
      for I in Held'Range loop
         Small_Tables.Insert (T, Cap (CAP_NOTIFICATION, READ_ONLY, Unsigned_64 (I), 1), Inserted);
         Check (Inserted.Status = Small_Tables.Opened, "fill");
         Held (I) := Inserted.Opened_Handle;
      end loop;
      -- Refused admission and refused derivation change nothing.
      Before := Small_Tables.Live_Count (T);
      Small.Place (T, (Admitted => False, Why => ABI.Sealed), Source, Req (READ_ONLY), Placed);
      Check (Placed.Status = Small.Refused and then Placed.Why = ABI.Sealed
             and then Small_Tables.Live_Count (T) = Before, "refused admission");
      Small.Place (T, Admit_Yes, Cap (CAP_REPLY, ALL_RIGHTS, 5, 0), Req (READ_ONLY), Placed);
      Check (Placed.Status = Small.Refused and then Placed.Why = ABI.Not_Transferable
             and then Small_Tables.Live_Count (T) = Before, "refused derivation");
      -- An installation adds exactly the copy; every earlier entry stays.
      Small.Place (T, Admit_Yes, Source, Req (READ_WRITE, Set_Tag => True, Tag => 77), Placed);
      Check (Placed.Status = Small.Placed, "placed");
      if Placed.Status = Small.Placed then
         Lookup := Small_Tables.Lookup (T, Placed.Handle);
         Check (Lookup.Check = Small_Tables.Current
                and then Lookup.Item = Cap (CAP_ENDPOINT, READ_WRITE, 5, 0, 77),
                "placed copy resolves");
      end if;
      for I in Held'Range loop
         Lookup := Small_Tables.Lookup (T, Held (I));
         Check (Lookup.Check = Small_Tables.Current
                and then Lookup.Item = Cap (CAP_NOTIFICATION, READ_ONLY, Unsigned_64 (I), 1),
                "earlier entry untouched");
      end loop;
      -- The limit (11) is reached now.
      Small.Place (T, Admit_Yes, Source, Req (READ_ONLY), Placed);
      Check (Placed.Status = Small.Refused and then Placed.Why = ABI.Limit_Reached,
             "limit reached");
      Small_Tables.Release_All (T);

      -- Out of memory on the first page.
      Fake_Pages.Reset;
      Small_Tables.Configure (T, 2, (Kind => Small_Tables.Kernel_Limit));
      Fake_Pages.Set_Budget (0);
      Small.Place (T, Admit_Yes, Source, Req (READ_ONLY), Placed);
      Check (Placed.Status = Small.Refused and then Placed.Why = ABI.Out_Of_Memory
             and then Small_Tables.Live_Count (T) = 0, "out of memory");
      Fake_Pages.Clear_Budget;
      Small_Tables.Release_All (T);
      Check (Fake_Pages.Held_Total = 0, "pages returned");
   end Test_Placement;

   ---------------------------------------------------------------------------
   procedure Test_Random is
      subtype Pick is Natural range 0 .. 1_000_000;
      package Random is new Ada.Numerics.Discrete_Random (Pick);
      G : Random.Generator;
      function Small_Word return Unsigned_64 is
        (case Random.Random (G) mod 6 is
            when 0 => 0,
            when 1 => Unsigned_64 (Random.Random (G) mod 200),
            when 2 => Unsigned_64'Last - Unsigned_64 (Random.Random (G) mod 4),
            when 3 => 2 ** 30 + Unsigned_64 (Random.Random (G) mod 3) - 1,
            when 4 => Unsigned_64 (Random.Random (G)) * 4096,
            when others => Unsigned_64 (Random.Random (G) mod 70_000));
      function Rights return CapabilityRights is
        (Rights_Of (Unsigned_64 (Random.Random (G) mod 32)));
      Agreements : Natural := 0;
      Accepted   : Natural := 0;
   begin
      Random.Reset (G, 20261013);
      for N in 1 .. 400_000 loop
         declare
            Kind : constant CapabilityType :=
              CapabilityType'Val (Random.Random (G) mod
                                  (CapabilityType'Pos (CapabilityType'Last) + 1));
            Source : constant Capability :=
              Cap (Kind, Rights, Small_Word, Small_Word,
                   (if Random.Random (G) mod 2 = 0 then 0 else Small_Word),
                   Generation (Random.Random (G) mod 5));
            Item : constant Request :=
              Req (Rights,
                   Set_Tag => Random.Random (G) mod 4 = 0,
                   Tag     => (if Random.Random (G) mod 3 = 0 then 0 else Small_Word),
                   Narrow  => Random.Random (G) mod 2 = 0,
                   Base    => Small_Word,
                   Length  => Small_Word);
            O : constant Outcome := Derive (Source, Item);
         begin
            Check (O.Accepted = Reference.Derivable (Source, Item), "differential");
            if O.Accepted then
               Accepted := Accepted + 1;
               Check (Is_Derivation (O.Copy, Source), "copy is a derivation");
               Check (isSubsetOf (O.Copy.rights, Source.rights), "rights never widen");
            end if;
            Agreements := Agreements + 1;
         end;
      end loop;
      Put_Line ("random derivations:" & Agreements'Image & " (accepted" &
                Accepted'Image & ")");
      Check (Accepted > 1_000, "the random test reaches acceptances");
   end Test_Random;

begin
   Test_Derive;
   Test_Decode;
   Test_Admission;
   Test_Placement;
   Test_Random;
   Put_Line ("capability-install:" & Checks'Image & " checks," &
             Failures'Image & " failed");
   if Failures /= 0 then
      raise Program_Error with "capability-install tests failed";
   end if;
end Main;
