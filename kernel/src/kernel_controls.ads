-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Control messages (Stop, Interrupt, Reload) kept for their target until
-- it reads them (docs/ipc-delivery.md, "Control is per capability").
--
-- @description
-- A target's state lives on the target process's record (no table indexed
-- by process number: KERN-003). It has a few sender slots. A sender's
-- messages are kept in its own slot, apart from every other sender's: two
-- senders' Stops are two facts. A repeat from the same sender (this life)
-- of a kind not yet read is the same fact. When every slot holds another
-- sender's unread messages, Send says Busy and the sender keeps its
-- message: nothing is accepted and then lost.
--
-- Callers serialise each target's state (Process.IPC.controlLock).
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPC_Labels; use IPC_Labels;

package Kernel_Controls with
    SPARK_Mode => On
is
    -- A sender, as the kernel names processes (Process.ProcessID).
    type Sender_Id is new Natural;
    No_Sender : constant Sender_Id := 0;

    Sender_Slots : constant := 8;
    subtype Slot_Index is Positive range 1 .. Sender_Slots;

    type Kind_Set is array (Control_Kind) of Boolean;
    No_Kinds : constant Kind_Set := (others => False);

    type Sender_Slot is record
        Sender     : Sender_Id := No_Sender;
        Generation : Unsigned_64 := 0;
        Pending    : Kind_Set := No_Kinds;
    end record;
    type Slot_Array is array (Slot_Index) of Sender_Slot;

    type Target_State is record
        Open       : Boolean := False;
        Generation : Unsigned_64 := 0;
        Slots      : Slot_Array;
    end record;

    function In_Use (S : Sender_Slot) return Boolean is
      (S.Pending /= No_Kinds);

    function Holds
      (T : Target_State; Sender : Sender_Id; Generation : Unsigned_64;
       Kind : Control_Kind) return Boolean is
      (for some I in Slot_Index =>
         T.Slots (I).Sender = Sender and then
         T.Slots (I).Generation = Generation and then
         T.Slots (I).Pending (Kind));

    -- A sender has at most one slot in use, so its facts are in one place.
    function Valid (T : Target_State) return Boolean is
      (for all I in Slot_Index =>
         (for all J in Slot_Index =>
            (if I /= J and then In_Use (T.Slots (I)) and then In_Use (T.Slots (J))
             then T.Slots (I).Sender /= T.Slots (J).Sender or else
                  T.Slots (I).Generation /= T.Slots (J).Generation)));

    function Any_Pending (T : Target_State) return Boolean is
      (for some I in Slot_Index => In_Use (T.Slots (I)));

    type Send_Result is (Accepted, Busy, Not_Open);

    -- The target (this life) takes control messages from now.
    procedure Open (T : out Target_State; Generation : Unsigned_64)
      with Post => Valid (T) and then T.Open and then
                   T.Generation = Generation and then not Any_Pending (T);

    -- The target ended: what it had not read goes with it.
    procedure Close (T : in out Target_State)
      with Post => Valid (T) and then not T.Open and then not Any_Pending (T);

    procedure Send
      (T : in out Target_State; Target_Generation : Unsigned_64;
       Sender : Sender_Id; Sender_Generation : Unsigned_64; Kind : Control_Kind;
       Result : out Send_Result)
      with Pre  => Valid (T) and then Sender /= No_Sender,
           Post => Valid (T) and then
                   (if not T'Old.Open or else T'Old.Generation /= Target_Generation
                    then Result = Not_Open) and then
                   (if Result = Accepted then Holds (T, Sender, Sender_Generation, Kind)
                    else T = T'Old) and then
                   -- Nothing kept is lost: every slot in use keeps its
                   -- sender and all it held.
                   (for all I in Slot_Index =>
                      (if In_Use (T'Old.Slots (I)) then
                         T.Slots (I).Sender = T'Old.Slots (I).Sender and then
                         T.Slots (I).Generation = T'Old.Slots (I).Generation and then
                         (for all K in Control_Kind =>
                            (if T'Old.Slots (I).Pending (K) then T.Slots (I).Pending (K)))));

    -- The target takes one of its unread messages, if any.
    procedure Take
      (T : in out Target_State; Sender : out Sender_Id;
       Sender_Generation : out Unsigned_64; Kind : out Control_Kind;
       Found : out Boolean)
      with Pre  => Valid (T),
           Post => Valid (T) and then
                   Found = Any_Pending (T'Old) and then
                   (if Found then
                      Holds (T'Old, Sender, Sender_Generation, Kind) and then
                      not Holds (T, Sender, Sender_Generation, Kind));

end Kernel_Controls;
