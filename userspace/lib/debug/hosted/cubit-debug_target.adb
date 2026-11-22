------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with CuBit.Debug_Protocol;

package body CuBit.Debug_Target is
   package P renames CuBit.Debug_Protocol;

   PT_Load : constant := 1;
   PT_Note : constant := 4;
   Phoff_At : constant := 32;
   Phentsize_At : constant := 54;
   Phnum_At : constant := 56;
   GNU_Build_Id : constant := 3;
   Hosted_Name : constant String := "hosted";

   function Hosted (Program, Source_Root : String) return Process_Handle is
      H : Process_Handle;
   begin
      H.Program (1 .. Program'Length) := Program;
      H.Program_Length := Program'Length;
      H.Source_Root (1 .. Source_Root'Length) := Source_Root;
      H.Source_Root_Length := Source_Root'Length;
      return H;
   end Hosted;

   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is (Milliseconds);

   procedure Load_Path (Path : String; Data : out Buffer_Access; Result : out Status) is
      use Ada.Streams.Stream_IO;
      F : File_Type;
   begin
      Data := null;
      if not Ada.Directories.Exists (Path) then
         Result := Not_Found;
         return;
      end if;
      Open (F, In_File, Path);
      declare
         Length : constant Natural := Natural (Size (F));
         subtype Chunk is Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Length));
         Bytes : Chunk;
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Read (F, Bytes, Last);
         Data := new Byte_Buffer (0 .. Length - 1);
         for I in Data'Range loop
            Data (I) := Unsigned_8 (Bytes (Ada.Streams.Stream_Element_Offset (I + 1)));
         end loop;
      end;
      Close (F);
      Result := Ok;
   end Load_Path;

   function U (Data : Byte_Buffer; At_Byte : Unsigned_64; Size : Natural) return Unsigned_64 is
      V : Unsigned_64 := 0;
   begin
      if At_Byte + Unsigned_64 (Size) > Unsigned_64 (Data'Length) then
         return 0;
      end if;
      for I in reverse 0 .. Size - 1 loop
         V := Shift_Left (V, 8) or Unsigned_64 (Data (Natural (At_Byte) + I));
      end loop;
      return V;
   end U;

   procedure Put (Page : in out State_Page; At_Byte : Natural; V : Unsigned_64; Size : Natural) is
   begin
      for I in 0 .. Size - 1 loop
         Page (At_Byte + I) := Unsigned_8 (Shift_Right (V, 8 * I) and 16#FF#);
      end loop;
   end Put;

   procedure Connect
     (Handle : Process_Handle; Until_Time : Unsigned_64;
      C : out Connection_Access; Result : out Status)
   is
      pragma Unreferenced (Until_Time);
      Phoff, Phentsize, Phnum : Unsigned_64;
   begin
      C := new Connection;
      C.Handle := Handle;
      Load_Path (Handle.Program (1 .. Handle.Program_Length), C.Image, Result);
      if Result /= Ok then
         return;
      end if;
      Phoff := U (C.Image.all, Phoff_At, 8);
      Phentsize := U (C.Image.all, Phentsize_At, 2);
      Phnum := U (C.Image.all, Phnum_At, 2);
      Put (C.State, P.Magic_At, Unsigned_64 (P.Magic), 4);
      Put (C.State, P.Version_At, Unsigned_64 (P.Version), 2);
      for I in 0 .. Phnum - 1 loop
         declare
            Base : constant Unsigned_64 := Phoff + I * Phentsize;
            Kind : constant Unsigned_64 := U (C.Image.all, Base, 4);
         begin
            if Kind = PT_Load and then C.Segment_Count < Segment_Ceiling then
               C.Segment_Count := C.Segment_Count + 1;
               C.Segments (C.Segment_Count) :=
                 (Address => U (C.Image.all, Base + 16, 8), File_Offset => U (C.Image.all, Base + 8, 8),
                  File_Size => U (C.Image.all, Base + 32, 8), Memory_Size => U (C.Image.all, Base + 40, 8));
            elsif Kind = PT_Note then
               declare
                  Off : constant Unsigned_64 := U (C.Image.all, Base + 8, 8);
               begin
                  if U (C.Image.all, Off + 8, 4) = GNU_Build_Id
                    and then U (C.Image.all, Off + 4, 4) = P.Build_Id_Ceiling
                  then
                     Put (C.State, P.Build_Id_Length_At, P.Build_Id_Ceiling, 4);
                     for J in 0 .. P.Build_Id_Ceiling - 1 loop
                        C.State (P.Build_Id_At + J) := C.Image (Natural (Off) + 16 + J);
                     end loop;
                  end if;
               end;
            end if;
         end;
      end loop;
      for I in Hosted_Name'Range loop
         C.State (P.Name_At + I - 1) := Character'Pos (Hosted_Name (I));
      end loop;
      for I in Hosted_Name'Length .. P.Name_Bytes - 1 loop
         C.State (P.Name_At + I) := Character'Pos (' ');
      end loop;
      Result := Ok;
   end Connect;

   procedure Disconnect (C : in out Connection_Access; Until_Time : Unsigned_64) is
      pragma Unreferenced (Until_Time);
   begin
      C := null;
   end Disconnect;

   procedure Read_State (C : Connection; Page : out State_Page) is
   begin
      Page := C.State;
   end Read_State;

   procedure Read
     (C : in out Connection; Address : Unsigned_64; Into : out Byte_Buffer; Got : out Natural) is
   begin
      Into := [others => 0];
      Got := 0;
      for I in Into'Range loop
         declare
            A : constant Unsigned_64 := Address + Unsigned_64 (I - Into'First);
            Found : Boolean := False;
         begin
            for S of C.Segments (1 .. C.Segment_Count) loop
               if A >= S.Address and then A < S.Address + S.Memory_Size then
                  Into (I) := (if A - S.Address < S.File_Size
                               then C.Image (Natural (S.File_Offset + A - S.Address)) else 0);
                  Found := True;
                  exit;
               end if;
            end loop;
            exit when not Found;
            Got := Got + 1;
         end;
      end loop;
   end Read;

   procedure Pause (C : Connection; Until_Time : Unsigned_64; Epoch : out Unsigned_64; Result : out Status) is
      pragma Unreferenced (C, Until_Time);
   begin
      Epoch := 1;
      Result := Ok;
   end Pause;

   procedure Resume (C : Connection; Until_Time : Unsigned_64; Result : out Status) is
      pragma Unreferenced (C, Until_Time);
   begin
      Result := Ok;
   end Resume;

   procedure Load_Debug_File
     (C : Connection; Build_Id : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Path : out String; Path_Length : out Natural; Result : out Status)
   is
      pragma Unreferenced (Build_Id, Until_Time);
      Name : constant String := C.Handle.Program (1 .. C.Handle.Program_Length);
   begin
      Path := [others => ' '];
      Path_Length := Natural'Min (Name'Length, Path'Length);
      Path (Path'First .. Path'First + Path_Length - 1) := Name (Name'First .. Name'First + Path_Length - 1);
      Load_Path (Name, Data, Result);
   end Load_Debug_File;

   procedure Load_Source
     (C : Connection; Build_Id : String; File : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Result : out Status)
   is
      pragma Unreferenced (Build_Id, Until_Time);
   begin
      Load_Path (C.Handle.Source_Root (1 .. C.Handle.Source_Root_Length) & "/" & File, Data, Result);
   end Load_Source;
   --  The hosted target has no kernel: no threads, no registers.
   function Through_Kernel (C : Connection) return Boolean is
      pragma Unreferenced (C);
   begin
      return False;
   end Through_Kernel;

   function Target_Identity (C : Connection) return Unsigned_64 is
      pragma Unreferenced (C);
   begin
      return 0;
   end Target_Identity;

   procedure Kernel_Threads
     (C : Connection; List : out Kernel_Thread_Array; Count : out Kernel_Thread_Count;
      Live : out Natural; Result : out Status) is
      pragma Unreferenced (C);
   begin
      List := [others => <>];
      Count := 0;
      Live := 0;
      Result := Refused;
   end Kernel_Threads;

   procedure Kernel_Registers
     (C : Connection; Thread : Kernel_Thread; Regs : out General_Registers;
      Result : out Register_Status) is
      pragma Unreferenced (C, Thread);
   begin
      Regs := [others => 0];
      Result := Failed;
   end Kernel_Registers;

   procedure Refresh (C : in out Connection) is
      pragma Unreferenced (C);
   begin
      null;
   end Refresh;

end CuBit.Debug_Target;
