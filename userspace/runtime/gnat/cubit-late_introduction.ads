------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Late introduction (ABI-002, CAP-003 run-time binding): a running
--  program is introduced to a service that started after it, for an
--  optional service request its manifest declared.
--
--  @description
--  At launch procmgr resolves each optional service entry of a program's
--  manifest with the same checks as a required one (declared, approved,
--  startup-only roles approved by the plan). When the only thing missing is
--  the provider (nothing runs that provides it), the entry is Absent in the
--  program's startup table and procmgr keeps the decision it made: the
--  entry's index, the provider role, the rights and the badge it would
--  have minted. The program's startup table then also holds the endpoint
--  named Binding: procmgr's own endpoint, badged with Introducer_Tag.
--
--  Over it, Await asks to be introduced (a call accepting a capability):
--  words (Index_Word) = the startup-table index of the Absent entry.
--  procmgr answers at once when the provider runs (Introduced, carrying the
--  endpoint as it would have been minted at launch), refuses at once
--  (Not_Declared) when it kept no decision for that caller and index, and
--  otherwise keeps the call and answers it when a provider for that role
--  starts. Each decision is answered once; the call never carries anything
--  the program's manifest did not declare.
--
--  Synchronous calls only go down (IPC DAG): the program calls procmgr;
--  procmgr never calls the program, it only answers a kept call.
--
--  This package is the pure wire encoding.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Late_Introduction with Pure, SPARK_Mode is

   --  The startup-table name of the introducer endpoint.
   Binding : constant String := "introductions";

   --  procmgr's endpoint badge for it. ASCII "INTRODUC".
   Introducer_Tag : constant Unsigned_64 := 16#494E_5452_4F44_5543#;

   Await_Request : constant Unsigned_32 := 16#0263#;
   Await_Words   : constant := 1;
   Index_Word    : constant := 0;

   --  The answers. Introduced carries the endpoint.
   Introduced   : constant Unsigned_32 := 16#F000#;
   --  procmgr kept no decision for this caller and index.
   Not_Declared : constant Unsigned_32 := 16#F002#;
   --  The decision was dropped without an introduction (the caller asked
   --  twice for one index, or procmgr could not mint it).
   Withdrawn    : constant Unsigned_32 := 16#F003#;

end CuBit.Late_Introduction;
