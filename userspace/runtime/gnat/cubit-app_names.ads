------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Who a window belongs to, for people (UI-061): Desktop asks procmgr for
--  the app name and identity a process's executable declares.
--
--  @description
--  An executable's manifest may declare its name for people
--  (app => (App_Declaration.Declared (App_Presentation name => "Files"))).
--  The manifest tool writes it into the identity section (.cubit.id) as a
--  "name" pair beside "id" and "version", so it is signed with the
--  executable; procmgr reads it when it launches the process and keeps it
--  with the identity. Nothing a process sends at run time changes it: a
--  window titles itself, but its task button names the app.
--
--  Describe: words (0) a memory grant reference (generation << 32 | slot)
--  of one writable page the caller owns, (1) the process identity
--  (KERN-003). Only the registered Desktop may ask. procmgr fills the page
--  (Record layout below) and replies label and word 0 a Status.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.App_Names with Pure, SPARK_Mode is
   Describe_Operation : constant := 16#0116#;
   Request_Words : constant := 2;
   Grant_Word : constant := 0;
   Process_Word : constant := 1;

   --  OK: the record describes the process (its name may be empty: the
   --  manifest declares none). Unknown: procmgr did not launch it or it is
   --  gone.
   type Status is (OK, Denied, Invalid_Request, Unknown);
   for Status use (OK => 16#F000#, Denied => 16#F002#, Invalid_Request => 16#F003#,
                   Unknown => 16#F004#);

   --  The manifest's limits (CCL.Manifests.Model): a name is 1 .. 32
   --  printable characters, an identity 1 .. 64.
   Maximum_Name_Bytes : constant := 32;
   Maximum_Identity_Bytes : constant := 64;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Identity_Length is Natural range 0 .. Maximum_Identity_Bytes;

   --  The page, little-endian:
   --     0  u8  name length       1 .. 32  name
   --    33  u8  identity length  34 .. 97  identity
   Name_Length_Offset : constant := 0;
   Name_Offset : constant := 1;
   Identity_Length_Offset : constant := Name_Offset + Maximum_Name_Bytes;
   Identity_Offset : constant := Identity_Length_Offset + 1;
   Record_Bytes : constant := Identity_Offset + Maximum_Identity_Bytes;
   Page_Bytes : constant := 4_096;
   pragma Compile_Time_Error (Record_Bytes > Page_Bytes, "the describe record must fit its page");

   --  Whether Text could be a declared name: printable ASCII, no space at
   --  either end, no two in a row (as the manifest tool checks; Desktop
   --  checks again what it is told).
   function Valid_Name (Text : String) return Boolean is
     (Text'Length in 1 .. Maximum_Name_Bytes
      and then (for all C of Text => C in ' ' .. '~')
      and then Text (Text'First) /= ' ' and then Text (Text'Last) /= ' '
      and then (for all I in Text'First .. Text'Last - 1 =>
                  not (Text (I) = ' ' and then Text (I + 1) = ' ')));
end CuBit.App_Names;
