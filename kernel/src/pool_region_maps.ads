-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- One 2 MiB region of the process pool (KERN-014 slice 4,
-- docs/kernel-memory-map.md): which of its 512 frames are free, and which
-- free ones are known to be zero.
--
-- @description
-- The process pool holds regions the kernel's linear map no longer covers.
-- A run is 2**Order frames aligned to its size inside one region (DMA asks
-- for runs; a page asks for one frame). Clean frames need no zeroing before
-- a process gets them (a region joins the pool zeroed).
--
-- Proved (gnatprove level 2, tests/pool-region-maps): Find returns an
-- aligned run of set frames (free, or clean) inside the region; Take marks exactly that
-- run in use and not clean; Give marks exactly that run free and dirty;
-- nothing outside the run changes; a clean frame is always free. The
-- adapter's per-region free count is not part of the proof (hosted-tested).
-------------------------------------------------------------------------------
pragma Ada_2022;

package Pool_Region_Maps with SPARK_Mode, Pure is
   Frames_Per_Region : constant := 512;
   Max_Run_Order     : constant := 9;   -- a whole region

   subtype Frame_Offset is Natural range 0 .. Frames_Per_Region - 1;
   subtype Run_Order is Natural range 0 .. Max_Run_Order;

   type Bits is array (Frame_Offset) of Boolean;
   type Region_Map is record
      Free  : Bits := [others => False];
      Clean : Bits := [others => False];
   end record;

   -- 2**Order, spelled out so each run length is a literal for the prover.
   function Run_Frames (Order : Run_Order) return Positive is
     (case Order is
        when 0 => 1, when 1 => 2, when 2 => 4, when 3 => 8, when 4 => 16,
        when 5 => 32, when 6 => 64, when 7 => 128, when 8 => 256, when 9 => 512)
     with Post => Run_Frames'Result <= Frames_Per_Region;

   -- A run's first frame: a multiple of its length, and the run fits.
   function Aligned (First : Frame_Offset; Order : Run_Order) return Boolean is
     (First mod Run_Frames (Order) = 0
      and then First <= Frames_Per_Region - Run_Frames (Order));

   function In_Run (F, First : Frame_Offset; Order : Run_Order) return Boolean is
     (F >= First and then F - First < Run_Frames (Order))
     with Pre => Aligned (First, Order);

   function Run_Set (B : Bits; First : Frame_Offset; Order : Run_Order) return Boolean is
     (for all F in Frame_Offset => (if In_Run (F, First, Order) then B (F)))
     with Pre => Aligned (First, Order);

   function Run_Free (M : Region_Map; First : Frame_Offset; Order : Run_Order) return Boolean is
     (Run_Set (M.Free, First, Order))
     with Pre => Aligned (First, Order);

   function Run_Used (M : Region_Map; First : Frame_Offset; Order : Run_Order) return Boolean is
     (for all F in Frame_Offset => (if In_Run (F, First, Order) then not M.Free (F)))
     with Pre => Aligned (First, Order);

   -- Clean frames are free.
   function Valid (M : Region_Map) return Boolean is
     (for all F in Frame_Offset => (if M.Clean (F) then M.Free (F)));

   -- A region as it joins the pool: every frame free and clean.
   function Joined return Region_Map with
     Post => Valid (Joined'Result)
             and then (for all F in Frame_Offset =>
                         Joined'Result.Free (F) and then Joined'Result.Clean (F));

   function All_Free (M : Region_Map) return Boolean is
     (for all F in Frame_Offset => M.Free (F));

   -- An aligned run of 2**Order frames set in B (the free frames, or the
   -- clean ones: a clean frame is free), searched from Hint on.
   procedure Find
     (B : Bits; Order : Run_Order; Hint : Frame_Offset;
      First : out Frame_Offset; Found : out Boolean) with
     Post => (if Found then Aligned (First, Order) and then Run_Set (B, First, Order));

   procedure Take (M : in out Region_Map; First : Frame_Offset; Order : Run_Order) with
     Pre  => Valid (M) and then Aligned (First, Order) and then Run_Free (M, First, Order),
     Post => Valid (M) and then Run_Used (M, First, Order)
             and then (for all F in Frame_Offset =>
                         (if In_Run (F, First, Order) then not M.Clean (F)
                          else M.Free (F) = M'Old.Free (F) and then M.Clean (F) = M'Old.Clean (F)));

   procedure Give (M : in out Region_Map; First : Frame_Offset; Order : Run_Order) with
     Pre  => Valid (M) and then Aligned (First, Order) and then Run_Used (M, First, Order),
     Post => Valid (M) and then Run_Free (M, First, Order)
             and then (for all F in Frame_Offset =>
                         (if In_Run (F, First, Order) then not M.Clean (F)
                          else M.Free (F) = M'Old.Free (F) and then M.Clean (F) = M'Old.Clean (F)));

   -- A free frame zeroed (through the kernel window).
   procedure Mark_Clean (M : in out Region_Map; F : Frame_Offset) with
     Pre  => Valid (M) and then M.Free (F),
     Post => Valid (M) and then M.Clean (F) and then M.Free = M'Old.Free
             and then (for all G in Frame_Offset => (if G /= F then M.Clean (G) = M'Old.Clean (G)));
end Pool_Region_Maps;
