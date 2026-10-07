------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Grant_References;
with CuBit.Kernel_ABI; use CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Launch_Arguments;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;
with CuBit.Libc_Imports; use CuBit.Libc_Imports;
with CuBit.Program_Descriptions;
with CuBit.Stream_Regions;
with CuBit.Stream_Rings;

package body CuBit.Libc_Child_Outlets is

   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type System.Address;

   package LA renames CuBit.Launch_Arguments;
   package PD renames CuBit.Program_Descriptions;
   use type PD.Connector_Direction;
   package GR renames CuBit.Grant_References;
   package Regions renames CuBit.Stream_Regions;
   package SR renames CuBit.Stream_Rings;

   Stderr_Outlet : constant String := "unix.stderr";
   Stderr_Descriptor : constant := 2;
   --  The largest ring lent, in declared pages; a program declaring more
   --  gets none (and keeps its own).
   Maximum_Ring_Pages : constant := 16;
   --  A slot's pages: the region for the largest ring.
   Slot_Pages : constant := 1 + Maximum_Ring_Pages;
   --  One record copied at a time: the largest a lent ring holds (half of
   --  it; Stream_Rings.Fits).
   Copy_Bytes : constant := Maximum_Ring_Pages * Page_Bytes / 2;
   Copy : array (1 .. Copy_Bytes) of Unsigned_8;

   function Write (Descriptor : int; Data : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.long
   with Import, Convention => C, External_Name => "write";


   --  A slot: its pages (Slot_Pages, kept for the next child), the ring's
   --  declared pages, the grant they were last lent under, and the child
   --  watching them.
   type Slot_State is record
      Base       : Unsigned_64 := 0;            --  0: no pages yet
      Pages      : Natural := 0;                --  declared, of the ring lent
      Grant      : GR.Reference := (slot => 0, generation => 1);
      Lent       : Boolean := False;            --  grant not yet seen retired
      Watched    : Boolean := False;
      Process    : int := 0;
   end record;
   --  Zero-filled (.bss): no pages, nothing lent or watched.
   Slots : array (1 .. Maximum_Forwarded) of Slot_State;
   pragma Suppress_Initialization (Slots);

   --  The description answer area (OP_PROGRAM_DESCRIPTION writes the
   --  descriptor after the name), lent writable to procmgr once.
   Answer_Bytes : constant := LA.Maximum_Name_Bytes + PD.Maximum_Descriptor_Bytes;
   Answer_Pages : constant := (Answer_Bytes + Page_Bytes - 1) / Page_Bytes;
   Answer : System.Address := System.Null_Address;
   Answer_Grant : Unsigned_64 := 0;

   --  Lend a fresh Pages-page area to procmgr with Flags: its address and
   --  grant's wire form, or Null_Address.
   procedure Lend_Area (Pages : Positive; Flags : Unsigned_64;
                        Area : out System.Address; Wire : out Unsigned_64);
   procedure Lend_Area (Pages : Positive; Flags : Unsigned_64;
                        Area : out System.Address; Wire : out Unsigned_64) is
      Slot, Generation : Unsigned_64;
      Ignore : int;
   begin
      Wire := 0;
      Area := mmap (System.Null_Address, Interfaces.C.size_t (Pages * Page_Bytes),
                    int (PROT_READ + PROT_WRITE), int (MAP_PRIVATE + MAP_ANONYMOUS), -1, 0);
      if Area = MAP_FAILED then
         Area := System.Null_Address;
         return;
      end if;
      Slot := CuBit.Kernel_Calls.Call
        (Create_Shared_Memory_Grant_Via_Capability, Process_Manager_Slot,
         Unsigned_64 (To_Integer (Area)), Unsigned_64 (Pages), Flags);
      Generation := (if Slot = Failed then 0
                     else CuBit.Kernel_Calls.Call (Get_Owned_Shared_Memory_Grant_Generation, Slot));
      if Slot = Failed or else Generation = Failed or else Generation = 0 then
         Ignore := munmap (Area, Interfaces.C.size_t (Pages * Page_Bytes));
         Area := System.Null_Address;
         return;
      end if;
      Wire := Shift_Left (Generation, Generation_Shift) or Slot;
   end Lend_Area;

   --  Program's description, decoded (OP_PROGRAM_DESCRIPTION).
   procedure Describe (Program : String; S : out PD.Signature; Ok : out Boolean);
   procedure Describe (Program : String; S : out PD.Signature; Ok : out Boolean) is
      Empty : constant PD.Bytes (1 .. 0) := [others => 0];
      Decoded : Boolean;
   begin
      --  An empty signature unless a description decodes below.
      PD.Decode (Empty, S, Decoded);
      Ok := False;
      if Program'Length not in 1 .. LA.Maximum_Name_Bytes then
         return;
      end if;
      if Answer = System.Null_Address then
         Lend_Area (Answer_Pages, Grant_Read_Write, Answer, Answer_Grant);
         if Answer = System.Null_Address then
            return;
         end if;
      end if;
      declare
         Area : String (1 .. Answer_Bytes) with Import, Address => Answer;
         M : aliased Message :=
           (Label => PD.Description_Operation, Length => PD.Description_Request_Words,
            Words => [Answer_Grant, Unsigned_64 (Program'Length), 0, 0],
            others => <>);
         Label : Unsigned_32;
      begin
         Area (1 .. Program'Length) := Program;
         Label := Unsigned_32 (CuBit.Kernel_Calls.Call
           (Call_Via_Endpoint_Capability, Process_Manager_Slot,
            Unsigned_64 (To_Integer (M'Address))) and 16#FFFF_FFFF#);
         if Label /= Reply_OK or else M.Words (0) not in 1 .. PD.Maximum_Descriptor_Bytes then
            return;
         end if;
         declare
            Length : constant Positive := Positive (M.Words (0));
            Descriptor : PD.Bytes (1 .. Length);
         begin
            for K in Descriptor'Range loop
               Descriptor (K) := Character'Pos (Area (Program'Length + K));
            end loop;
            PD.Decode (Descriptor, S, Ok);
         end;
      end;
   end Describe;

   --  A slot free for a ring: none watching it, its last grant retired.
   function Free_Slot return Slot_Choice;
   function Free_Slot return Slot_Choice is
   begin
      for K in Slots'Range loop
         declare
            S : Slot_State renames Slots (K);
         begin
            if not S.Watched then
               if S.Lent and then GR.Retirement_Confirmed
                 (S.Grant, CuBit.Kernel_Calls.Call
                             (Get_Owned_Shared_Memory_Grant_Generation, S.Grant.slot))
               then
                  S.Lent := False;
               end if;
               if not S.Lent then
                  return K;
               end if;
            end if;
         end;
      end loop;
      return No_Slot;
   end Free_Slot;

   procedure Prepare (Program : String; Rings : out CuBit.Outlet_Rings.Table;
                      Slot : out Slot_Choice)
   is
      S : PD.Signature;
      Ok, Found : Boolean;
      Index : PD.Connector_Index;
   begin
      Rings := (others => <>);
      Slot := No_Slot;
      Describe (Program, S, Ok);
      if not Ok then
         return;
      end if;
      PD.Find_Connector (S, Stderr_Outlet, Index, Found);
      if not Found or else S.Connectors (Index).Direction /= PD.Outlet then
         return;
      end if;
      declare
         Pages : constant Natural := S.Connectors (Index).Pages;
         Chosen : constant Slot_Choice :=
           (if Pages in 1 .. Maximum_Ring_Pages then Free_Slot else No_Slot);
         Wire : Unsigned_64;
         Area : System.Address;
      begin
         if Chosen = No_Slot then
            return;
         end if;
         declare
            T : Slot_State renames Slots (Chosen);
            Slot_Number, Generation : Unsigned_64;
         begin
            if T.Base = 0 then
               --  The slot's pages, for this child and the ones after it.
               Area := mmap (System.Null_Address,
                             Interfaces.C.size_t (Slot_Pages * Page_Bytes),
                             int (PROT_READ + PROT_WRITE),
                             int (MAP_PRIVATE + MAP_ANONYMOUS), -1, 0);
               if Area = MAP_FAILED then
                  return;
               end if;
               T.Base := Unsigned_64 (To_Integer (Area));
            end if;
            T.Pages := Pages;
            Regions.Initialize (T.Base, SR.ELEMENT_TEXT_LINE);
            Slot_Number := CuBit.Kernel_Calls.Call
              (Create_Shared_Memory_Grant_Via_Capability, Process_Manager_Slot,
               T.Base, Unsigned_64 (SR.Region_Pages (Regions.Declared (Pages))),
               Grant_Forwardable_Read_Write);
            if Slot_Number = Failed then
               return;
            end if;
            Generation := CuBit.Kernel_Calls.Call
              (Get_Owned_Shared_Memory_Grant_Generation, Slot_Number);
            if Generation = Failed or else Generation = 0 then
               return;
            end if;
            T.Grant := (slot => Slot_Number, generation => Generation);
            T.Lent := True;
            Wire := GR.Encode (T.Grant);
            Rings.Count := 1;
            Rings.Entries (1) := (Outlet => Index, Grant => Wire);
            Slot := Chosen;
         end;
      end;
   end Prepare;

   procedure Give_Back (Slot : Slot_Choice);
   procedure Give_Back (Slot : Slot_Choice) is
      Ignore : Unsigned_64;
   begin
      if Slot = No_Slot or else not Slots (Slot).Lent then
         return;
      end if;
      Ignore := CuBit.Kernel_Calls.Call
        (Revoke_Shared_Memory_Grant_Reference, Slots (Slot).Grant.slot,
         Slots (Slot).Grant.generation);
      Slots (Slot).Watched := False;
      Slots (Slot).Process := 0;
   end Give_Back;

   procedure Started (Slot : Slot_Choice; Process : int) is
   begin
      if Slot /= No_Slot then
         Slots (Slot).Watched := True;
         Slots (Slot).Process := Process;
      end if;
   end Started;

   procedure Abandon (Slot : Slot_Choice) is
   begin
      Give_Back (Slot);
   end Abandon;

   --  Copy the records in Slot's ring to descriptor 2 (the owner's read in
   --  place).
   procedure Forward (Slot : Slot_Choice);
   procedure Forward (Slot : Slot_Choice) is
      Length : Natural;
      Ignore : Interfaces.C.long;
   begin
      loop
         Length := Regions.Read_Owned (Slots (Slot).Base, Slots (Slot).Pages, Copy'Address,
                                       Copy'Length);
         exit when Length = 0;
         Ignore := Write (Stderr_Descriptor, Copy'Address, Interfaces.C.size_t (Length));
      end loop;
   end Forward;

   procedure Forward_All is
   begin
      for K in Slots'Range loop
         if Slots (K).Watched then
            Forward (K);
         end if;
      end loop;
   end Forward_All;

   procedure Ended (Process : int) is
   begin
      for K in Slots'Range loop
         if Slots (K).Watched and then Slots (K).Process = Process then
            Forward (K);
            Give_Back (K);
         end if;
      end loop;
   end Ended;

   function Any_Watched return Boolean is
     (for some S of Slots => S.Watched);

end CuBit.Libc_Child_Outlets;
