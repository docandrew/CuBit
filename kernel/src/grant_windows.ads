-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Received-grant windows (KERN-003 step 2b, docs/process-objects.md): the
-- slots of a grantee's address space that grants are mapped into.
--
-- @description
-- Each grantee has a set of windows in its received region. Creating a
-- grant takes a free window from the grantee's set, and the kernel
-- returns the address (ACQUIRE). The window goes back only after the
-- grant's pages are unmapped and every CPU has flushed them. Where a
-- grant is mapped no longer depends on its table index, so userspace
-- never computes it and the table can grow independently of the region.
--
-- A grant larger than one window takes a run of consecutive windows
-- (Allocate_Run), so a grant's size is bounded by its owner's memory (the
-- pages it shares are already its own) and the grantee's free windows, not
-- by a per-grant constant.
--
-- Proved: an allocation takes windows that were free and changes no other,
-- and a release frees exactly its windows. Not proved: that Allocate or
-- Allocate_Run finds windows whenever enough are free (callers refuse the
-- grant if not; regression-tested).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Grant_Windows with
    SPARK_Mode => On,
    Pure
is
    -- Windows per grantee: how many grants it may hold mapped at once.
    Window_Count : constant := 2 ** 14;
    subtype Window is Natural range 0 .. Window_Count - 1;

    type Window_Set is private;
    Empty : constant Window_Set;

    function Contains (S : Window_Set; W : Window) return Boolean;

    -- No window is taken.
    function Is_Empty (S : Window_Set) return Boolean
      with Post => (if Is_Empty'Result then (for all W in Window => not Contains (S, W)));

    procedure Allocate (S : in out Window_Set; W : out Window; Found : out Boolean)
      with Post =>
        (if Found then
           not Contains (S'Old, W) and then Contains (S, W) and then
           (for all V in Window => (if V /= W then Contains (S, V) = Contains (S'Old, V)))
         else S = S'Old);

    procedure Release (S : in out Window_Set; W : Window)
      with Pre  => Contains (S, W),
           Post => not Contains (S, W) and then
                   (for all V in Window => (if V /= W then Contains (S, V) = Contains (S'Old, V)));

    -- Runs of Count consecutive windows starting at First.
    subtype Run_Length is Positive range 1 .. Window_Count;
    function Fits (First : Window; Count : Run_Length) return Boolean is
      (Count <= Window_Count - First);
    function In_Run (V, First : Window; Count : Run_Length) return Boolean is
      (V >= First and then V - First < Count);

    procedure Allocate_Run
      (S : in out Window_Set; Count : Run_Length; First : out Window; Found : out Boolean)
      with Post =>
        (if Found then
           Fits (First, Count) and then
           (for all V in Window =>
              (if In_Run (V, First, Count) then not Contains (S'Old, V) and then Contains (S, V)
               else Contains (S, V) = Contains (S'Old, V)))
         else S = S'Old);

    procedure Release_Run (S : in out Window_Set; First : Window; Count : Run_Length)
      with Pre  => Fits (First, Count) and then
                   (for all V in Window => (if In_Run (V, First, Count) then Contains (S, V))),
           Post => (for all V in Window =>
                      (if In_Run (V, First, Count) then not Contains (S, V)
                       else Contains (S, V) = Contains (S'Old, V)));

private
    Word_Bits : constant := 64;
    subtype Word_Index is Natural range 0 .. Window_Count / Word_Bits - 1;
    subtype Bit_Index is Natural range 0 .. Word_Bits - 1;
    type Word_Array is array (Word_Index) of Unsigned_64;
    type Window_Set is record
        Words : Word_Array := [others => 0];
    end record;
    Empty : constant Window_Set := (Words => [others => 0]);

    function Bit (B : Bit_Index) return Unsigned_64 is (Shift_Left (1, B));

    function Contains (S : Window_Set; W : Window) return Boolean is
      ((S.Words (W / Word_Bits) and Bit (W mod Word_Bits)) /= 0);
end Grant_Windows;
