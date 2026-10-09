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
-- Proved: an allocation takes a window that was free and changes no other,
-- and a release frees exactly the one window. Not proved: that Allocate
-- finds a window whenever one is free (callers refuse the grant if not;
-- regression-tested).
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
