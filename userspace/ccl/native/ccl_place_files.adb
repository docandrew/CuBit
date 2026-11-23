------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Files in places through the filesystem service (see the spec).
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Memory_Grants;
with CuBit.Handles;
with CCL_Native_Bindings;

package body CCL_Place_Files is
   PAGE_BYTES : constant := 4_096;
   BUFFER_PAGES : constant := BUFFER_BYTES / PAGE_BYTES;
   pragma Compile_Time_Error
     (BUFFER_BYTES < 2 * MAXIMUM_PATH_BYTES, "a rename's two paths must fit the buffer");

   --  CuAlloc, page-aligned (the loan's pages), kept for the program's life.
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";

   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer : Unsigned_64 := 0;
   Ready, Tried : Boolean := False;

   function Initialize return Boolean is
   begin
      if Ready or else Tried then
         return Ready;
      end if;
      Tried := True;
      if not CuBit.Handles.Valid (CCL_Native_Bindings.Filesystem) then
         return False;
      end if;
      Buffer := Allocate_Zeroed (BUFFER_BYTES, PAGE_BYTES);
      if Buffer = 0 then
         return False;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (CCL_Native_Bindings.Filesystem, To_Address (Integer_Address (Buffer)), BUFFER_PAGES, True, Loan, Ready);
      return Ready;
   end Initialize;

   function Outcome (Label : Unsigned_32) return Result_Kind is
     (case Label is
         when REPLY_OK => Done,
         when REPLY_NOT_FOUND => Not_Found,
         when REPLY_ACCESS_DENIED | REPLY_READ_ONLY => Not_Granted,
         when REPLY_ALREADY_EXISTS => Already_Exists,
         when REPLY_IS_DIRECTORY | REPLY_WRONG_OBJECT_TYPE => Wrong_Kind,
         when REPLY_NOT_EMPTY => Not_Empty,
         when REPLY_NO_SPACE => No_Space,
         when REPLY_VOLUME_UNAVAILABLE => Unavailable,
         when others => Failed);

   procedure Put_Text (At_Offset : Natural; Text : String) is
      Bytes : String (1 .. Text'Length)
        with Import, Address => To_Address (Integer_Address (Buffer + Unsigned_64 (At_Offset)));
   begin
      Bytes := Text;
   end Put_Text;

   function Call (Request : in out Message) return Result_Kind is
   begin
      Request.tag := capCall (CCL_Native_Bindings.Filesystem, Request, CuBit.Messages.Wait_Forever);
      return Outcome (Request.tag.label);
   end Call;

   --  OPEN's word layout: UNLINK, MKDIR and RMDIR use it.
   function Path_Request (Label : Unsigned_32; Length : Natural) return Message is
     (tag => (label => Label, length => 4, protocol => 0, ipc => <>, reserved => 0),
      authorityTag => 0,
      words => [0 => Loan.slot, 1 => Unsigned_64 (Length), 2 => 0, 3 => Loan.generation]);

   function Usable (Path : String) return Boolean is
     (Path'Length in 1 .. MAXIMUM_PATH_BYTES and then Initialize);

   procedure Open (Path : String; Options : Open_Options; Item : out File; Result : out Result_Kind) is
      Request : Message;
   begin
      Item := No_File;
      if not Usable (Path) then
         Result := (if Ready then Failed else Unavailable);
         return;
      end if;
      Put_Text (0, Path);
      Request := Open_Request (Loan, Path'Length, Options);
      Result := Call (Request);
      if Result = Done then
         Item := (Handle => Request.words (0), Open => True);
      end if;
   end Open;

   procedure Create (Path : String; Exclusive : Boolean; Item : out File; Result : out Result_Kind) is
   begin
      Open (Path, OPEN_READ_WRITE or OPEN_CREATE or
              (if Exclusive then OPEN_EXCLUSIVE else OPEN_TRUNCATE), Item, Result);
   end Create;

   procedure Open_Read (Path : String; Item : out File; Result : out Result_Kind) is
   begin
      Open (Path, OPEN_READ_ONLY, Item, Result);
   end Open_Read;

   procedure Write
     (Item : File; Offset : Unsigned_64; Data : System.Address; Count : Natural; Result : out Result_Kind)
   is
      Done_Bytes : Natural := 0;
      Request : Message;
   begin
      Result := (if Item.Open and then Ready then Done else Failed);
      while Result = Done and then Done_Bytes < Count loop
         declare
            N : constant Natural := Natural'Min (BUFFER_BYTES, Count - Done_Bytes);
            Source : String (1 .. N) with Import, Address => Data + Storage_Offset (Done_Bytes);
         begin
            Put_Text (0, Source);
            Request := Write_At_Request
              (File_Handle (Item.Handle), Loan, Unsigned_64 (N), Offset + Unsigned_64 (Done_Bytes));
            Result := Call (Request);
            if Result = Done and then Request.words (0) /= Unsigned_64 (N) then
               Result := No_Space;
            end if;
            Done_Bytes := Done_Bytes + N;
         end;
      end loop;
   end Write;

   procedure Read
     (Item : File; Offset : Unsigned_64; Data : System.Address; Count : Natural; Got : out Natural;
      Result : out Result_Kind)
   is
      Request : Message;
   begin
      Got := 0;
      Result := (if Item.Open and then Ready then Done else Failed);
      while Result = Done and then Got < Count loop
         declare
            Wanted : constant Natural := Natural'Min (BUFFER_BYTES, Count - Got);
            N : Natural;
         begin
            Request := Read_At_Request (File_Handle (Item.Handle), Loan, Unsigned_64 (Wanted),
                                        Offset + Unsigned_64 (Got));
            Result := Call (Request);
            exit when Result /= Done;
            N := Natural (Unsigned_64'Min (Request.words (0), Unsigned_64 (Wanted)));
            exit when N = 0;
            declare
               Target : String (1 .. N) with Import, Address => Data + Storage_Offset (Got);
               Source : constant String (1 .. N)
                 with Import, Address => To_Address (Integer_Address (Buffer));
            begin
               Target := Source;
            end;
            Got := Got + N;
            exit when N < Wanted;
         end;
      end loop;
   end Read;

   procedure Close (Item : in out File) is
      Request : Message;
      Ignored : Result_Kind;
   begin
      if Item.Open then
         Request := Close_Request (File_Handle (Item.Handle));
         Ignored := Call (Request);
      end if;
      Item := No_File;
   end Close;

   procedure Path_Call (Label : Unsigned_32; Path : String; Result : out Result_Kind) is
      Request : Message;
   begin
      if not Usable (Path) then
         Result := (if Ready then Failed else Unavailable);
         return;
      end if;
      Put_Text (0, Path);
      Request := Path_Request (Label, Path'Length);
      Result := Call (Request);
   end Path_Call;

   procedure Make_Directory (Path : String; Result : out Result_Kind) is
   begin
      Path_Call (OP_MKDIR, Path, Result);
   end Make_Directory;

   procedure Remove_File (Path : String; Result : out Result_Kind) is
   begin
      Path_Call (OP_UNLINK, Path, Result);
   end Remove_File;

   procedure Remove_Directory (Path : String; Result : out Result_Kind) is
   begin
      Path_Call (OP_RMDIR, Path, Result);
   end Remove_Directory;

   procedure Rename (From, To : String; Result : out Result_Kind) is
      Request : Message;
   begin
      if not Usable (From) or else To'Length not in 1 .. MAXIMUM_PATH_BYTES then
         Result := (if Ready then Failed else Unavailable);
         return;
      end if;
      Put_Text (0, From);
      Put_Text (From'Length, To);
      Request := Rename_Request (Loan, From'Length, To'Length);
      Result := Call (Request);
   end Rename;

   function Exists (Path : String) return Boolean is
      Item : File;
      Result : Result_Kind;
      Request : Message;
   begin
      Open (Path, OPEN_READ_ONLY, Item, Result);
      if Result = Done then
         Close (Item);
         return True;
      elsif Result = Wrong_Kind then
         return True;
      elsif not Ready or else Path'Length > MAXIMUM_PATH_BYTES then
         return False;
      end if;
      --  A directory: opened as one, then closed.
      Put_Text (0, Path);
      Request := Open_Directory_Request (Loan, Path'Length);
      Result := Call (Request);
      if Result = Done then
         Request := Close_Directory_Request (Directory_Handle (Request.words (0)));
         Result := Call (Request);
         return True;
      end if;
      return False;
   end Exists;
end CCL_Place_Files;
