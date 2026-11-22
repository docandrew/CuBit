------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the paired host did, kept for the person at the device: the
--  ccl-remote window lists it (newest at the bottom) and each entry is also
--  a "ccl-remote: host <op> ..." line in logstore.
--
--  @description
--  One entry per operation the host performs: when (monotonic ms), what
--  (Kind), on what (the CCL source of an evaluation, the place and name of
--  a file operation, the component of a build) and how it ended. Arguments
--  are kept as one printable line, at most Maximum_Argument_Shown
--  characters, longer ones cut with Ellipsis. Nothing secret passes
--  through here: the protocol's only secret is the pairing token, which is
--  never an argument.
--
--  The entries live in a ring that starts at Initial_Entries and doubles
--  (on the process heap, CuAlloc) as the host does more, up to the named
--  ceiling Maximum_Entries; past it the oldest entry gives way, so the
--  ring never takes more than Maximum_Entries entries of heap.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;

package Remote_Activity is
   --  What the host did, as the person reads it.
   type Kind is (Hello, Evaluate, List, Stat, Get, Put, Remove, Build, Mail, Capture, App);
   type Outcome is (OK, Refused, Failed);
   function Kind_Name (Item : Kind) return String is
     (case Item is
         when Hello => "hello", when Evaluate => "evaluate", when List => "list",
         when Stat => "stat", when Get => "get", when Put => "put", when Remove => "rm",
         when Build => "build", when Mail => "mail", when Capture => "capture",
         when App => "app");
   function Outcome_Name (Item : Outcome) return String is
     (case Item is when OK => "ok", when Refused => "refused", when Failed => "failed");

   --  An argument as shown: one line, cut to this many characters (the
   --  last Ellipsis'Length of them then being the ellipsis).
   Maximum_Argument_Shown : constant := 96;
   Ellipsis : constant String := "...";
   subtype Argument_Length is Natural range 0 .. Maximum_Argument_Shown;
   subtype Argument_Text is String (1 .. Maximum_Argument_Shown);

   --  Text as one printable line: controls (newlines, tabs) become spaces,
   --  runs of spaces one space, and longer text ends in Ellipsis.
   function Shown (Text : String) return String
     with Post => Shown'Result'Length <= Maximum_Argument_Shown;

   type Activity is record
      Time_Ms : Interfaces.Unsigned_64 := 0;
      Op : Kind := Hello;
      Result : Outcome := OK;
      Argument : Argument_Text := [others => ' '];
      Argument_Last : Argument_Length := 0;
   end record;
   function Argument_Of (Item : Activity) return String is
     (Item.Argument (1 .. Item.Argument_Last));

   --  The ring: its first allocation, and the ceiling it grows to.
   Initial_Entries : constant := 16;
   Maximum_Entries : constant := 512;
   subtype Entry_Count is Natural range 0 .. Maximum_Entries;
   --  1 is the oldest entry kept, Count the newest.
   subtype Entry_Number is Positive range 1 .. Maximum_Entries;

   procedure Add
     (Time_Ms : Interfaces.Unsigned_64; Op : Kind; Argument : String; Result : Outcome);
   function Count return Entry_Count;
   function Entry_At (Number : Entry_Number) return Activity
     with Pre => Number <= Count;
   --  Entries added since the start (including those that gave way).
   function Added return Interfaces.Unsigned_64;
   --  The ring's current size (tests): Initial_Entries .. Maximum_Entries,
   --  or 0 before the first entry.
   function Capacity return Entry_Count;

   --  One line, as the window and the log show it:
   --  "<op> <argument>: <outcome>" (no argument: "<op>: <outcome>").
   function Line (Item : Activity) return String;
   --  Time_Ms as an uptime clock: "H:MM:SS".
   function Clock_Image (Time_Ms : Interfaces.Unsigned_64) return String;

   --  Forget every entry and release the ring (tests).
   procedure Reset;
end Remote_Activity;
