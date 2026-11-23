------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  FAT32 cluster chains and free clusters (FS-030): walking a chain read
--  from an untrusted FAT, and the free-cluster ledger. Pure and proved
--  (SPARK).
--
--  @description
--  A chain on a corrupt volume may loop, end in a free or bad cluster, or
--  point past the data region. Every walk here stops: a chain of distinct
--  clusters has at most Clusters - 1 links, so a walk that would follow
--  one more has met a cluster twice (a cycle) and stops with Too_Long.
--  Each step's result says why a walk stopped; nothing past a bad link is
--  ever returned.
--
--  The ledger counts free clusters and keeps where the next search starts.
--  The FSInfo sector's counts are hints: the ledger's count comes from a
--  scan of the FAT at mount, and an allocation takes only a cluster whose
--  entry reads free.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Fat_Format; use Fat_Format;

package Fat_Chains with Pure, SPARK_Mode is

   type Step_Result is
     (Advanced,          --  followed one link
      Chain_End,         --  the entry ends the chain
      Free_In_Chain,     --  the entry reads free: the chain is broken
      Bad_In_Chain,      --  a bad-cluster mark inside a chain
      Invalid_In_Chain,  --  1, or a cluster past the data region
      Too_Long,          --  a cycle (a cluster met twice, or more links than clusters)
      Read_Failed);      --  the entry could not be read (the device)

   --  A walk keeps Brent's cycle check: a cluster it saved (at steps
   --  0, 1, 3, 7, ...) met again is a cycle, found within about twice the
   --  steps to the cycle and around it. The step bound alone also stops
   --  every walk.
   type Walk is record
      Current : Data_Cluster := First_Data_Cluster;
      Steps   : Cluster_Count := 0;   --  links followed
      Last    : Data_Cluster := First_Data_Cluster;   --  the volume's last cluster
      Saved   : Data_Cluster := First_Data_Cluster;
      Power   : Positive := 1;
      Lap     : Natural := 0;         --  steps since Saved
   end record;

   --  The walk is inside the volume, has followed at most as many links
   --  as a chain of distinct clusters can have, and its cycle check is
   --  where Brent's would be (Steps = Power - 1 + Lap).
   function Valid (W : Walk) return Boolean is
     (W.Current <= W.Last and then W.Saved <= W.Last
      and then W.Steps <= W.Last - First_Data_Cluster
      and then W.Lap < W.Power
      and then Long_Long_Integer (W.Steps) = Long_Long_Integer (W.Power) - 1 + Long_Long_Integer (W.Lap));

   function Start (First, Last : Data_Cluster) return Walk is
     (Current => First, Steps => 0, Last => Last, Saved => First, Power => 1, Lap => 0)
     with Pre => First <= Last, Post => Valid (Start'Result);

   --  One step, given the raw FAT entry of W.Current.
   procedure Step (W : in out Walk; Raw : Unsigned_32; Result : out Step_Result)
     with Pre  => Valid (W),
          Post => Valid (W) and then W.Last = W'Old.Last
                  and then (if Result = Advanced then W.Steps = W'Old.Steps + 1
                            else W = W'Old)
                  and then Result /= Read_Failed;

   --  Walks over a FAT the caller reads entry by entry (Read_Entry gives
   --  cluster C's raw entry, or OK False when the device failed).
   generic
      with procedure Read_Entry (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean);
   package Walks is

      --  The cluster Index links after First. Result is Advanced when it
      --  was reached (Index 0 is First itself); otherwise why not, and
      --  Found is the last cluster reached.
      procedure Seek
        (First, Last : Data_Cluster; Index : Cluster_Count;
         Found : out Data_Cluster; Reached : out Cluster_Count; Result : out Step_Result)
        with Pre  => First <= Last,
             Post => Found <= Last and then Reached <= Index
                     and then (if Result = Advanced then Reached = Index);

      --  The chain from First to its end: its length in clusters and its
      --  last cluster, Result Chain_End when it ends well.
      procedure Measure
        (First, Last : Data_Cluster; Length : out Cluster_Count; Tail : out Data_Cluster;
         Result : out Step_Result)
        with Pre  => First <= Last,
             Post => Tail <= Last and then Length >= 1 and then Result /= Advanced;
   end Walks;

   ---------------------------------------------------------------------------
   --  Free clusters
   ---------------------------------------------------------------------------
   type Ledger is record
      Total  : Cluster_Count := Fat32_Minimum_Clusters;
      Free   : Cluster_Count := 0;
      Cursor : Data_Cluster := First_Data_Cluster;   --  where the next search starts
   end record;

   function Valid (L : Ledger) return Boolean is
     (L.Total >= 1 and then L.Free <= L.Total and then L.Cursor <= L.Total + 1);

   function Last_Of (L : Ledger) return Data_Cluster is (L.Total + 1)
     with Pre => Valid (L);

   --  The ledger at mount: the counted free clusters, the search starting
   --  at the FSInfo hint when it has one (only a place to start).
   function Opening (Total : Cluster_Count; Counted : Cluster_Count; Hints : Info_Hints) return Ledger
     with Pre  => Total >= 1 and then Counted <= Total
                  and then (if Hints.Next_Known then Hints.Next <= Total + 1),
          Post => Valid (Opening'Result) and then Opening'Result.Free = Counted
                  and then Opening'Result.Total = Total;

   --  The cluster after C, wrapping to the first.
   function Following (L : Ledger; C : Data_Cluster) return Data_Cluster is
     (if C >= Last_Of (L) then First_Data_Cluster else C + 1)
     with Pre => Valid (L), Post => Following'Result <= Last_Of (L);

   procedure Take (L : in out Ledger; C : Data_Cluster)
     with Pre  => Valid (L) and then L.Free >= 1 and then C <= Last_Of (L),
          Post => Valid (L) and then L.Free = L'Old.Free - 1 and then L.Total = L'Old.Total
                  and then L.Cursor = Following (L'Old, C);

   procedure Give (L : in out Ledger)
     with Pre  => Valid (L) and then L.Free < L.Total,
          Post => Valid (L) and then L.Free = L'Old.Free + 1 and then L.Total = L'Old.Total
                  and then L.Cursor = L'Old.Cursor;

   Maximum_Sector_Bytes : constant := 2 ** Maximum_Sector_Shift;

   --  Free entries among Count entries of a FAT sector's bytes, from
   --  First_Entry (the decoded value 0; reserved top bits ignored).
   function Free_Entries (Data : Byte_Array; First_Entry, Count : Natural) return Natural
     with Pre  => Data'First = 0 and then Data'Length <= Maximum_Sector_Bytes
                  and then First_Entry <= Data'Length / Fat_Entry_Bytes
                  and then Count <= Data'Length / Fat_Entry_Bytes - First_Entry,
          Post => Free_Entries'Result <= Count;

   generic
      with procedure Read_Entry (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean);
   package Searches is
      --  The first free cluster from L.Cursor on (wrapping), looking at
      --  most at every cluster once. Found is False when none is free or
      --  an entry could not be read (Read_Failed).
      procedure Find_Free (L : Ledger; C : out Data_Cluster; Found : out Boolean; Failed : out Boolean)
        with Pre  => Valid (L),
             Post => C <= Last_Of (L) and then (if Found then not Failed);
   end Searches;

end Fat_Chains;
