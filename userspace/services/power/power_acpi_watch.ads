------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  power.svc's side of acpi.svc's power-source watch (PWR-004): the
--  request, and what each answer makes power.svc do. Pure, without I/O.
--
--  @description
--  acpi.svc's observer operation Watch (ACPI_Power_Watch_Protocol,
--  shared/acpi; tests/power-sources checks these numbers against it):
--  request [nonzero id, seen generation, 0, 0], answered [id, status,
--  generation, schema]. Seen 0 is answered at once with the current
--  generation; otherwise acpi.svc keeps the request and answers it when its
--  power inventory moves on (a Notify on a battery or adapter, a completed
--  publication, a changed sample, a family failure). The watch generation
--  is not the pages' cache generation: after Changed, power.svc reads the
--  pages again, then watches from the generation it was answered.
--
--  What each answer means here (Decide):
--    * Changed, with a generation above Seen: read, then watch from it;
--    * Superseded: a newer watch of this sender took its place (power.svc
--      keeps one, so it is watched again from Seen);
--    * Unavailable (acpi.svc is not serving its runtime yet, or is busy) and
--      Full (eight watches are kept): watched again after
--      Unavailable_Retry_Milliseconds. This re-registers a refused watch;
--      it never reads a source;
--    * Future_Generation: Seen is an earlier acpi.svc's; watch from 0;
--    * Stopping, Exhausted, Denied, Malformed, and any answer acpi.svc
--      never writes (another label or id, a schema, a Changed that does
--      not move on): the watch ends, visibly in the log.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Power_ACPI_Watch with Pure, SPARK_Mode is

   subtype Word is Unsigned_64;
   type Words is array (0 .. 3) of Word;

   --  ACPI_Power_Watch_Protocol.Watch and .Schema.
   Watch_Label : constant Unsigned_32 := 29;
   Schema : constant Word := 1;
   Request_Words : constant := 4;

   --  Request and reply positions.
   ID_Word         : constant := 0;
   Seen_Word       : constant := 1;
   Status_Word     : constant := 1;
   Generation_Word : constant := 2;
   Schema_Word     : constant := 3;

   --  A watch generation fits a signed CCL integer and never wraps.
   Maximum_Generation : constant Word := Word (Integer_64'Last);
   subtype Generation is Word range 0 .. Maximum_Generation;

   --  ACPI_Power_Watch_Protocol.Status, in its order (its representation
   --  is the position).
   type Status is
     (Changed, Superseded, Unavailable, Stopping, Exhausted, Denied, Malformed, Full, Future_Generation);

   --  How long after an Unavailable or Full answer the watch is submitted
   --  again.
   Unavailable_Retry_Milliseconds : constant := 1_000;

   type Step is
     (Read_Then_Watch,   --  the inventory moved on: read, then watch from New_Seen
      Watch_Again,       --  nothing to read: watch again from Seen
      Retry_Later,       --  refused for now: watch again after the retry interval
      Resynchronize,     --  an earlier acpi.svc's generation: watch from 0
      Give_Up);          --  the watch ends

   function Request (ID : Word; Seen : Generation) return Words is ([ID, Seen, 0, 0])
   with Pre => ID /= 0;

   function Valid_Status (W : Word) return Boolean is (W <= Status'Pos (Status'Last));

   --  The answer to the watch [ID, Seen]: Label, Length and Reply as
   --  received. Answered: it is an answer acpi.svc writes (Why is then its
   --  status).
   procedure Decide
     (Label : Unsigned_32; Length : Natural; Reply : Words; ID : Word; Seen : Generation;
      Next : out Step; New_Seen : out Generation; Why : out Status; Answered : out Boolean)
   with Post =>
     (case Next is
        when Read_Then_Watch => Answered and then Why = Changed and then New_Seen > Seen
                                and then New_Seen = Reply (Generation_Word),
        when Resynchronize   => Answered and then Why = Future_Generation and then New_Seen = 0,
        when Watch_Again     => Answered and then Why = Superseded and then New_Seen = Seen,
        when Retry_Later     => Answered and then Why in Unavailable | Full and then New_Seen = Seen,
        when Give_Up         => New_Seen = Seen
                                and then (if Answered then Why in Stopping | Exhausted | Denied | Malformed))
     and then (Answered = (Label = Watch_Label and then Length = Request_Words and then Reply (ID_Word) = ID
                           and then Reply (Schema_Word) = Schema and then Valid_Status (Reply (Status_Word))
                           and then Reply (Generation_Word) <= Maximum_Generation
                           and then (if Status'Val (Reply (Status_Word)) = Changed
                                     then Reply (Generation_Word) > Seen)));

   function Name (S : Status) return String is
     (case S is
        when Changed => "CHANGED", when Superseded => "SUPERSEDED", when Unavailable => "UNAVAILABLE",
        when Stopping => "STOPPING", when Exhausted => "EXHAUSTED", when Denied => "DENIED",
        when Malformed => "MALFORMED", when Full => "FULL", when Future_Generation => "FUTURE_GENERATION");
end Power_ACPI_Watch;
