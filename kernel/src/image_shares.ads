-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- KERN-006 layer 1: the kernel's table of shared images. Processes loaded
-- from the same image (Image_Share_Policy.Same, then the same bytes, which
-- the loader compares) map one set of frames for its read-only segments.
--
-- @description
-- An image's frames are the kernel's, not a process's: no process owns
-- them (BuddyAllocator's owner stays No_Owner), so no process can grant,
-- lend or DMA them, and a debug view reaches them only through Pin_View.
-- They are mapped read-only (text read+execute) and never writable, so
-- nothing is copy-on-write: writable segments are copied at load.
--
-- Charges, one explicit rule:
--  * physically once: the frames are charged to the image's own ledger
--    (a Memory_Accounting account opened for it), freed with the image;
--  * each process mapping the image is charged its pages in its own
--    quota, as if they were private (Process.Loader reserves them): a
--    quota never depends on what else runs, and is the same whether a
--    load shares or not. The process reads the shared part as
--    Shared_Image_Bytes in its readiness page, beside Owned_Bytes.
--
-- States: Free, Filling (the first loader copies the image into fresh
-- frames; nobody else finds it), Ready (others may join). Users counts the
-- processes mapping it; the last one to leave frees the frames (Release).
--
-- Locking: one leaf lock (Table_Lock) over the table; under it, only the
-- buddy allocator's lock (pins). Frames are freed with no lock held.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Virtmem;
with Image_Share_Policy;

package Image_Shares is
   package Policy renames Image_Share_Policy;

   -- Images shared at once: one per distinct program image running. A load
   -- finding the table full copies privately, as before.
   Capacity : constant := 256;
   type Share is range 0 .. Capacity;
   No_Share : constant Share := 0;
   subtype Live_Share is Share range 1 .. Capacity;

   -- A page of an image: its segments' pages in order, from 0.
   subtype Page_Index is Natural;

   -- Join the Ready image the same as K (Fresh False), or reserve a new
   -- one of Pages pages for the caller to fill (Fresh True; Set_Frame every
   -- page, then Publish). No_Share: none could be had (the table or the
   -- memory is full, or K takes no pages); the load copies privately.
   procedure Acquire (K : Policy.Key; Pages : Policy.Page_Count;
                      S : out Share; Fresh : out Boolean);

   function Pages (S : Live_Share) return Natural;

   -- The frame of one page (0 if not yet set).
   function Frame (S : Live_Share; Page : Page_Index) return Virtmem.PhysAddress
     with Pre => Page < Pages (S);

   -- While Filling, by the filler: a fresh, unpublished frame becomes
   -- page Page of the image, charged to the image's ledger (OK False: the
   -- charge could not be bound; the caller frees the frame).
   procedure Set_Frame (S : Live_Share; Page : Page_Index; Frame : Virtmem.PhysAddress;
                        OK : out Boolean)
     with Pre => Page < Pages (S);

   -- Filled: others may join now.
   procedure Publish (S : Live_Share);

   -- A user stops mapping the image (its tables no longer reach the
   -- frames, or never did). The last user frees every frame and the ledger.
   procedure Release (S : Live_Share);

   -- For a debug view of a target that maps S: pin page Page's frame if it
   -- is Frame and the image is still mapped by someone (Pinned False
   -- otherwise). The view holds the pin as it holds an owned frame's.
   procedure Pin_View (S : Live_Share; Page : Page_Index; Frame : Virtmem.PhysAddress;
                       Pinned : out Boolean);

   -- The image page at user address Address of a process mapping S
   -- (Inside False: none of S's segments holds it).
   procedure Page_Of (S : Live_Share; Address : Unsigned_64; Page : out Page_Index;
                      Inside : out Boolean);

   -- Why a load copied its read-only segments privately instead:
   --  Quota_Refused: the process's quota could not take its pages;
   --  No_Room: the table or the kernel's memory for it was full;
   --  Frame_Refused: no frame for a fresh image's page;
   --  Charge_Refused: a fresh frame's charge could not be bound;
   --  Source_Unreadable: the load's own bytes could not be read;
   --  Bytes_Differ: the image under the same key holds other bytes.
   type Fallback is (Quota_Refused, No_Room, Frame_Refused, Charge_Refused,
                     Source_Unreadable, Bytes_Differ);

   -- Say on the console that a load of Name was not shared, and Why, once
   -- per image (its key) and reason: "image not shared: NAME reason=WHY".
   procedure Note_Fallback (K : Policy.Key; Why : Fallback; Name : String);

   -- Images in the table and their frames (for the boot log and tests).
   procedure Statistics (Count, Frames : out Natural);

end Image_Shares;
