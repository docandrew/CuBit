-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Linked List implementation
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System;

with BuddyAllocator;
with Config;
with TextIO; use TextIO;

-- Ada implementation: custom storage, address overlays or live context state.
-- Only separately annotated SPARK policy/state routines carry proof obligations.
package body LinkedLists is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (capacity : in Natural)
    is
    begin
        SlabAllocator.setup (pool     => nodeSlab,
                             objSize  => Node'Size,
                             capacity => capacity);
    end setup;

    ---------------------------------------------------------------------------
    -- teardown
    ---------------------------------------------------------------------------
    procedure teardown
    is
    begin
        SlabAllocator.teardown (nodeSlab);
    end teardown;

    ---------------------------------------------------------------------------
    -- create
    ---------------------------------------------------------------------------
    procedure create (myList : in out List; capacity : in Natural)
    is
    begin
        myList.head := null;
        myList.tail := null;
        myList.length := 0;
        myList.capacity := capacity;
    end create;

    ---------------------------------------------------------------------------
    -- delete
    ---------------------------------------------------------------------------
    procedure delete (myList : in out List)
    is
    begin
        clear (myList);

        myList.head := null;
        myList.tail := null;
        myList.length := 0;
        myList.capacity := 0;
    end delete;

    ---------------------------------------------------------------------------
    -- insertFront
    ---------------------------------------------------------------------------
    procedure tryInsertFront
      (myList : in out List; element : in T; success : out Boolean)
    is
        use type System.Address;
        function To_Node is new Ada.Unchecked_Conversion (System.Address, NodePtr);
        address : System.Address;
        newNode : NodePtr;
    begin
        success := False;
        if myList.length >= myList.capacity then
            return;
        end if;

        SlabAllocator.tryAllocate (nodeSlab, address);
        if address = System.Null_Address then return; end if;
        -- Storage comes from the same pool used by free; no allocation or
        -- externally supplied pointer is hidden in this conversion.
        newNode := To_Node (address);
        newNode.all := (element => element, next => null, prev => null);

        if myList.length = 0 then
            newNode.prev    := newNode;
            newNode.next    := newNode;
            myList.head     := newNode;
            myList.tail     := newNode;
        else
            newNode.prev    := myList.tail;
            newNode.next    := myList.head;
            myList.head.prev := newNode;
            myList.tail.next := newNode;
            myList.head     := newNode;
        end if;

        myList.length := myList.length + 1;
        success := True;
    end tryInsertFront;

    procedure insertFront (myList : in out List; element : in T) is
        success : Boolean;
    begin
        tryInsertFront (myList, element, success);
        if not success then
            raise LinkedListException with "List capacity or node storage exhausted";
        end if;
    end insertFront;

    ---------------------------------------------------------------------------
    -- insertBack
    ---------------------------------------------------------------------------
    procedure insertBack (myList : in out List; element : in T)
    is
    begin
        insertFront (myList, element);
        -- Rotate the circular list to put the new head at its back.
        myList.tail := myList.head;
        myList.head := myList.head.next;
    end insertBack;

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (myList : in out List)
    is
        oldHead : NodePtr;
    begin
        if myList.length = 0 then
            raise ListEmptyException with "Cannot popFront on empty list";
        end if;

        oldHead         := myList.head;
        if myList.length = 1 then
            myList.head := null;
            myList.tail := null;
        else
            myList.head := oldHead.next;
            myList.head.prev := myList.tail;
            myList.tail.next := myList.head;
        end if;

        free (oldHead);
        myList.length := myList.length - 1;
    end popFront;

    ---------------------------------------------------------------------------
    -- front
    ---------------------------------------------------------------------------
    function front (myList : in List) return T
    is
    begin
        if myList.length = 0 then
            raise ListEmptyException with "Cannot get front of empty list";
        end if;

        return myList.head.element;
    end front;

    ---------------------------------------------------------------------------
    -- popBack
    ---------------------------------------------------------------------------
    procedure popBack (myList : in out List)
    is
        oldTail : NodePtr;
    begin
        if myList.length = 0 then
            raise ListEmptyException with "Cannot popBack of empty list";
        end if;

        oldTail         := myList.tail;
        if myList.length = 1 then
            myList.head := null;
            myList.tail := null;
        else
            myList.tail := oldTail.prev;
            myList.tail.next := myList.head;
            myList.head.prev := myList.tail;
        end if;

        free(oldTail);
        myList.length := myList.length - 1;
    end popBack;

    ---------------------------------------------------------------------------
    -- back
    ---------------------------------------------------------------------------
    function back (myList : in List) return T
    is
    begin
        if myList.length = 0 then
            raise ListEmptyException with "Cannot get back of empty list";
        end if;

        return myList.tail.element;
    end back;

    -- procedure remove(myList : in out List; value : in T) with
    --
    -- is
    --     curNode    : NodePtr := myList.head;
    --     prevNode   : NodePtr := curNode;
    -- begin
    --     if myList.length = 0 then
    --         raise ListEmptyException with "Cannot remove element from empty list";
    --     end if;

    -- end remove;

    ---------------------------------------------------------------------------
    -- clear
    ---------------------------------------------------------------------------
    procedure clear (myList : in out List)
    is
    begin
        DeleteLoop: loop
            exit DeleteLoop when myList.length = 0;
            popFront(myList);
        end loop DeleteLoop;
    end clear;

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (myList : in List)
    is
        curNode : NodePtr := myList.head;
    begin
        if myList.length = 0 then
            println("empty");
            return;
        end if;

        PrintLoop: loop
            printElem (curNode.element);
            exit PrintLoop when curNode = myList.tail;
            print (" -> ");
            curNode := curNode.next;
        end loop PrintLoop;

        println;
    end print;

end LinkedLists;
