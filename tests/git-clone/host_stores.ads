------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The Linux host's directories as a clone's store, for the hosted tests
--  (natively the store is the filesystem service).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Streams.Stream_IO;
with Inflate_Streams;

package Host_Stores is
   subtype Byte_Array is Inflate_Streams.Byte_Array;
   subtype Byte_Count is Inflate_Streams.Byte_Count;

   type Host_Store is limited record
      Output : Ada.Streams.Stream_IO.File_Type;
   end record;

   procedure Make_Directory (S : in out Host_Store; Path : String; Ok : out Boolean);
   procedure Create_File (S : in out Host_Store; Path : String; Ok : out Boolean);
   procedure Write (S : in out Host_Store; Offset : Byte_Count; Data : Byte_Array; Ok : out Boolean);
   procedure Close_File (S : in out Host_Store);
   procedure Remove_File (S : in out Host_Store; Path : String);
   procedure Remove_Directory (S : in out Host_Store; Path : String);
   procedure Rename (S : in out Host_Store; From, To : String; Ok : out Boolean);
   function Exists (S : in out Host_Store; Path : String) return Boolean;
end Host_Stores;
