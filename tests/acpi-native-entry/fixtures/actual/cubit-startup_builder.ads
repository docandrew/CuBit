------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The launcher's startup-table builder (CAP-002), shared by procmgr and
--  devmgr: from a program's declared startup table to the table its child
--  is given.
--
--  @description
--  Build takes the declaration (the program's .cubit.startup section,
--  decoded: every entry Unassigned) and asks Mint for each entry, in order.
--  Mint is the launcher's: it obtains the capability from its source
--  (procmgr's own authority, devmgr for device resources, a service's
--  policy endpoint), has the kernel place it in the child, and answers the
--  handle word or a typed refusal. Report is called once for every entry
--  Mint was asked for, with its outcome, so every grant and refusal is
--  logged.
--
--  * A required entry Mint refuses ends the build: Required_Refused names
--    the entry and why, Built is empty, and the launch must fail (the
--    launcher stops the child, which releases whatever was placed).
--  * An optional entry Mint refuses is recorded Absent.
--  * A Granted answer with a zero handle is a launcher bug: Minter_Fault.
--  * The kernel's two self capabilities are appended as "self" and
--    "self-process"; a declaration may not use those names.
--
--  Build is proved: when it answers Built, the table is valid and complete,
--  keeps the declaration's limit, names, kinds and needs in order, and
--  holds every required capability Granted.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Startup_Tables; use CuBit.Startup_Tables;

package CuBit.Startup_Builder with Pure, SPARK_Mode is

   --  What a source answers for one capability.
   type Mint_Outcome is
     (Granted, Refused_By_Policy, Not_Available, Timed_Out, Limit_Reached, Out_Of_Memory);
   subtype Mint_Refusal is Mint_Outcome range Refused_By_Policy .. Out_Of_Memory;

   type Build_Status is
     (Ready,
      Not_A_Declaration,       --  an entry is not Unassigned
      Too_Many_Entries,        --  no room for the two self entries
      Reserved_Name_Declared,  --  "self" or "self-process" declared
      Bad_Self_Handle,         --  a self handle is zero
      Required_Refused,        --  a required capability was refused
      Minter_Fault);           --  Granted with a zero handle

   type Build_Result is record
      Status : Build_Status := Not_A_Declaration;
      Index  : Entry_Count := 0;          --  the entry concerned, 0 if none
      Why    : Mint_Outcome := Granted;   --  Mint's answer for Index
   end record;

   --  Room for the self entries after the declared ones.
   Self_Entries : constant := 2;

   --  Built carries Declared's limit and, in order, its names, kinds and
   --  needs; every required one is Granted; then the two self entries.
   function Follows (Declared, Built : Table) return Boolean is
     (Declared.Count <= Maximum_Entries - Self_Entries
      and then Built.Count = Declared.Count + Self_Entries
      and then Built.Limit = Declared.Limit
      and then (for all I in 1 .. Declared.Count =>
                  Built.Entries (I).Label = Declared.Entries (I).Label
                  and then Built.Entries (I).Kind = Declared.Entries (I).Kind
                  and then Built.Entries (I).Requirement = Declared.Entries (I).Requirement
                  and then (if Declared.Entries (I).Requirement = Required
                            then Built.Entries (I).State = Granted))
      and then Built.Entries (Declared.Count + 1).Label = To_Name (Self_Text)
      and then Built.Entries (Declared.Count + 1).Kind = Endpoint
      and then Built.Entries (Declared.Count + 2).Label = To_Name (Self_Process_Text)
      and then Built.Entries (Declared.Count + 2).Kind = Process);

   generic
      type Context is limited private;
      with procedure Mint
        (Into : in out Context; Item : Startup_Entry; Index : Entry_Index;
         Handle : out Unsigned_64; Outcome : out Mint_Outcome);
      with procedure Report
        (Into : in out Context; Item : Startup_Entry; Index : Entry_Index;
         Outcome : Mint_Outcome);
   procedure Build
     (Into : in out Context; Declared : Table;
      Self_Handle, Self_Process_Handle : Unsigned_64;
      Built : out Table; Result : out Build_Result)
   with Pre  => Valid_Table (Declared),
        Post => (if Result.Status = Ready
                 then Valid_Table (Built) and then Complete (Built)
                      and then Follows (Declared, Built)
                      and then Result.Index = 0
                 else Built = Empty_Table)
                and then (if Result.Status in Required_Refused | Minter_Fault
                          then Result.Index in 1 .. Declared.Count
                               and then (if Result.Status = Required_Refused
                                         then Declared.Entries (Result.Index).Requirement = Required
                                              and then Result.Why in Mint_Refusal));

end CuBit.Startup_Builder;
