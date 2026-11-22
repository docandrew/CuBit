-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The buddy allocator's out-of-band free-list links (KERN-014 slice 1,
-- docs/kernel-memory-map.md): one 32-bit code per link, naming either a
-- free block by its head frame or the list head (sentinel) of one order.
--
-- @description
-- The free lists used to live in the free blocks themselves; a block a
-- process gave back then carried kernel addresses, and a frame outside the
-- kernel's map could not hold them at all. Each frame now has two codes
-- (next, previous) in a table beside the block descriptors.
--
-- Proved (gnatprove level 2, tests/free-link-codes): a frame code and a
-- list code never coincide; decoding returns exactly what was encoded; the
-- link pair's layout fits the metadata entry.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Free_Link_Codes with SPARK_Mode, Pure is
   -- List heads: one per buddy order. Orders are bounded far below this.
   List_Count : constant := 64;
   subtype List_Index is Natural range 0 .. List_Count - 1;

   type Code is new Unsigned_32;
   First_List_Code : constant Code := Code'Last - (List_Count - 1);
   -- Frames the codes can name: every frame below 16 TiB but the last
   -- List_Count (the linear map's limit is 16 TiB, Virtmem).
   subtype Frame_Number is Unsigned_64 range 0 .. Unsigned_64 (First_List_Code) - 1;

   function Of_Frame (Frame : Frame_Number) return Code is (Code (Frame))
     with Post => Of_Frame'Result < First_List_Code;
   function Of_List (List : List_Index) return Code is
     (First_List_Code + Code (List))
     with Post => Of_List'Result >= First_List_Code;

   function Is_List (Item : Code) return Boolean is (Item >= First_List_Code);

   function Frame_Of (Item : Code) return Frame_Number is (Frame_Number (Item))
     with Pre => not Is_List (Item);
   function List_Of (Item : Code) return List_Index is
     (List_Index (Item - First_List_Code))
     with Pre => Is_List (Item);

   -- Both links of one frame, as stored in its metadata entry (8 bytes).
   type Links is record
      Next, Previous : Code := 0;
   end record with Size => 64, Object_Size => 64;
   for Links use record
      Next     at 0 range 0 .. 31;
      Previous at 4 range 0 .. 31;
   end record;
   Link_Bytes : constant := 8;

   procedure Prove_Round_Trips (Frame : Frame_Number; List : List_Index) with
     Ghost,
     Post => not Is_List (Of_Frame (Frame))
       and then Frame_Of (Of_Frame (Frame)) = Frame
       and then Is_List (Of_List (List))
       and then List_Of (Of_List (List)) = List
       and then Of_Frame (Frame) /= Of_List (List);

   procedure Prove_Injective (Left, Right : Frame_Number;
                              Left_List, Right_List : List_Index) with
     Ghost,
     Post => (Of_Frame (Left) = Of_Frame (Right)) = (Left = Right)
       and then (Of_List (Left_List) = Of_List (Right_List)) = (Left_List = Right_List);
end Free_Link_Codes;
