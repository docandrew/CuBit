------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The host's directories as a store (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Directories;

package body Host_Stores is
   use Ada.Streams;
   use type Inflate_Streams.Byte_Count;
   package Dirs renames Ada.Directories;

   procedure Make_Directory (S : in out Host_Store; Path : String; Ok : out Boolean) is
      pragma Unreferenced (S);
   begin
      Ok := not Dirs.Exists (Path);
      if Ok then
         Dirs.Create_Directory (Path);
      end if;
   exception
      when others => Ok := False;
   end Make_Directory;

   procedure Create_File (S : in out Host_Store; Path : String; Ok : out Boolean) is
   begin
      Ok := not Dirs.Exists (Path);
      if Ok then
         Stream_IO.Create (S.Output, Stream_IO.Out_File, Path);
      end if;
   exception
      when others => Ok := False;
   end Create_File;

   procedure Write (S : in out Host_Store; Offset : Byte_Count; Data : Byte_Array; Ok : out Boolean) is
   begin
      Stream_IO.Set_Index (S.Output, Stream_IO.Positive_Count (Offset + 1));
      Byte_Array'Write (Stream_IO.Stream (S.Output), Data);
      Ok := True;
   exception
      when others => Ok := False;
   end Write;

   procedure Close_File (S : in out Host_Store) is
   begin
      Stream_IO.Close (S.Output);
   end Close_File;

   procedure Remove_File (S : in out Host_Store; Path : String) is
      pragma Unreferenced (S);
   begin
      Dirs.Delete_File (Path);
   exception
      when others => null;
   end Remove_File;

   procedure Remove_Directory (S : in out Host_Store; Path : String) is
      pragma Unreferenced (S);
   begin
      Dirs.Delete_Directory (Path);
   exception
      when others => null;
   end Remove_Directory;

   procedure Rename (S : in out Host_Store; From, To : String; Ok : out Boolean) is
      pragma Unreferenced (S);
   begin
      Dirs.Rename (From, To);
      Ok := True;
   exception
      when others => Ok := False;
   end Rename;

   function Exists (S : in out Host_Store; Path : String) return Boolean is
      pragma Unreferenced (S);
   begin
      return Dirs.Exists (Path);
   end Exists;

end Host_Stores;
