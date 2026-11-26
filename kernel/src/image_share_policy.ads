-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- KERN-006 layer 1, the rules: processes loaded from the same image share
-- the frames of its read-only segments.
--
-- @description
-- What may be shared: a PT_LOAD segment that is read-only (R or R+X, never
-- W) and starts on a page. Writable segments are copied at load, as before;
-- nothing is ever copy-on-write.
--
-- When two loads are the same image (Same): the same entry point and
-- read-only segments (address, file offset, file and memory sizes,
-- executable or not), and then (the loader checks it, page by page,
-- before mapping anything) the same bytes in every shared page. The key
-- only finds a candidate; the bytes decide. So a spawner can never put
-- other bytes behind an image another spawner loaded (no poisoning), and
-- sharing never needs a hash.
--
-- Users: how many processes map an image's frames. The frames go when the
-- last one does (Leave answers Last).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Image_Share_Policy with SPARK_Mode => On, Pure is

   Page_Bytes : constant := 4096;

   -- An image's read-only segments the kernel shares: Ladybird's and the
   -- native programs' links have two (text, rodata); more are loaded
   -- privately, as before.
   Maximum_Segments : constant := 8;
   type Segment_Count is range 0 .. Maximum_Segments;
   subtype Segment_Index is Segment_Count range 1 .. Maximum_Segments;

   -- ELF program-header flags (ELF.PF_*), as the loader reads them.
   Flag_Execute : constant := 1;
   Flag_Write   : constant := 2;
   Flag_Read    : constant := 4;

   -- Pages in one image: the loader's frame count is a Natural.
   subtype Page_Count is Unsigned_64 range 0 .. Unsigned_64 (Natural'Last);

   type Segment is record
      Address      : Unsigned_64 := 0;
      Offset       : Unsigned_64 := 0;
      File_Bytes   : Unsigned_64 := 0;
      Memory_Bytes : Unsigned_64 := 0;
      Executable   : Boolean := False;
   end record;
   type Segment_Array is array (Segment_Index) of Segment;

   type Key is record
      Entry_Point : Unsigned_64 := 0;
      Count       : Segment_Count := 0;
      Segments    : Segment_Array := [others => <>];
   end record;

   -- A segment the kernel may share: read-only, on a page, not empty, its
   -- file part within its memory part (ELF_Admission checked the image).
   function Shareable (Flags : Unsigned_32; Address, File_Bytes, Memory_Bytes : Unsigned_64)
     return Boolean is
     ((Flags = Flag_Read or else Flags = (Flag_Read or Flag_Execute))
      and then Address mod Page_Bytes = 0
      and then Memory_Bytes > 0
      and then File_Bytes <= Memory_Bytes
      and then Memory_Bytes <= Unsigned_64'Last - (Page_Bytes - 1))
   with Post => (if Shareable'Result then (Flags and Flag_Write) = 0);

   function Pages_Of (S : Segment) return Unsigned_64 is
     ((S.Memory_Bytes + (Page_Bytes - 1)) / Page_Bytes)
   with Pre => S.Memory_Bytes <= Unsigned_64'Last - (Page_Bytes - 1);

   -- A well-formed key: every used segment shareable (so never writable).
   function Valid (K : Key) return Boolean is
     (for all I in 1 .. K.Count =>
        K.Segments (I).Memory_Bytes > 0
        and then K.Segments (I).Memory_Bytes <= Unsigned_64'Last - (Page_Bytes - 1)
        and then K.Segments (I).File_Bytes <= K.Segments (I).Memory_Bytes
        and then K.Segments (I).Address mod Page_Bytes = 0);

   -- Two loads of the same image, as far as the key tells.
   function Same (A, B : Key) return Boolean is
     (A.Entry_Point = B.Entry_Point
      and then A.Count = B.Count
      and then (for all I in 1 .. A.Count => A.Segments (I) = B.Segments (I)));

   -- The pages all of K's segments need (Fits: representable as a Page_Count).
   procedure Total_Pages (K : Key; Pages : out Page_Count; Fits : out Boolean)
     with Pre => Valid (K),
          Post => (if Fits then Pages > 0 or else K.Count = 0);

   -- Users of one image's frames.
   Maximum_Users : constant := 65_536;
   type User_Count is range 0 .. Maximum_Users;

   -- Another process maps the image (Joined False: it is full; then the
   -- load copies privately).
   procedure Join (Users : in out User_Count; Joined : out Boolean)
     with Post => (if Users'Old < User_Count'Last
                   then Joined and then Users = Users'Old + 1
                   else not Joined and then Users = Users'Old);

   -- A process stops mapping it: Last when it was the only one (the
   -- frames go now).
   procedure Leave (Users : in out User_Count; Last : out Boolean)
     with Pre  => Users > 0,
          Post => Users = Users'Old - 1 and then Last = (Users = 0);

end Image_Share_Policy;
