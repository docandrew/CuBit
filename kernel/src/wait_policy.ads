-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- WAIT's decision (ABI-002 slice 2): which form a Sources word and a
-- deadline name, or that they name none. No queue access; Syscall.IPC
-- performs the form with the mailbox, completion, futex and timer
-- primitives the old entry points used.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Wait_ABI;

package Wait_Policy with SPARK_Mode => On, Pure is
    package ABI renames Wait_ABI;

    type Form is
      (Messages_Forever,  -- {Requests, Events}, Forever: block until one
       Messages_Until,    -- {Requests, Events}, a deadline: until then
       Messages_Now,      -- {Requests, Events}, Now: look once
       Requests_Now,      -- {Requests}, Now
       Events_Now,        -- {Events}, Now
       Completions_Wait,  -- {Completions}, Forever: at least C of B
       Completions_Now,   -- {Completions}, Now: up to B, C = 0
       Ready,             -- {Requests, Events, Completions} + Readiness_Only
       Futex,             -- {Futex_Word} + modifiers (ABI.Futex_Form), any deadline
       Offered,           -- {Offered_Capability}, Now
       Timer,             -- {Timer}, Forever
       Unsupported);

    function Decide (Sources, Deadline, B, C : Unsigned_64) return Form
    with Post =>
      (case Decide'Result is
          when Messages_Forever =>
              Sources = ABI.Messages_Sources and then Deadline = ABI.Forever,
          when Messages_Until =>
              Sources = ABI.Messages_Sources and then Deadline /= ABI.Forever
              and then Deadline /= ABI.Now,
          when Messages_Now =>
              Sources = ABI.Messages_Sources and then Deadline = ABI.Now,
          when Requests_Now =>
              Sources = ABI.Requests_Sources and then Deadline = ABI.Now,
          when Events_Now =>
              Sources = ABI.Events_Sources and then Deadline = ABI.Now,
          when Completions_Wait =>
              Sources = ABI.Completions_Sources and then Deadline = ABI.Forever
              and then B /= 0 and then C <= B,
          when Completions_Now =>
              Sources = ABI.Completions_Sources and then Deadline = ABI.Now
              and then B /= 0 and then C = 0,
          when Ready => Sources = ABI.Ready_Sources,
          when Futex => ABI.Futex_Form (Sources),
          when Offered =>
              Sources = ABI.Offered_Sources and then Deadline = ABI.Now,
          when Timer =>
              Sources = ABI.Timer_Sources and then Deadline = ABI.Forever,
          when Unsupported =>
              -- Exactly the words no form takes.
              not (Sources = ABI.Messages_Sources
                   or else (Sources = ABI.Requests_Sources and then Deadline = ABI.Now)
                   or else (Sources = ABI.Events_Sources and then Deadline = ABI.Now)
                   or else (Sources = ABI.Completions_Sources and then B /= 0
                            and then ((Deadline = ABI.Forever and then C <= B)
                                      or else (Deadline = ABI.Now and then C = 0)))
                   or else Sources = ABI.Ready_Sources
                   or else ABI.Futex_Form (Sources)
                   or else (Sources = ABI.Offered_Sources and then Deadline = ABI.Now)
                   or else (Sources = ABI.Timer_Sources and then Deadline = ABI.Forever)));

    -- A form that may block the caller.
    function Blocks (F : Form) return Boolean is
      (F in Messages_Forever | Messages_Until | Completions_Wait | Ready | Futex | Timer);

end Wait_Policy;
