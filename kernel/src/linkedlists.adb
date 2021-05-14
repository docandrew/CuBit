-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Linked List implementation
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with BuddyAllocator;
with Config;
with TextIO; use TextIO;

package body LinkedLists
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (capacity : in Natural) with
        SPARK_Mode => Off
    is
    begin
        SlabAllocator.setup (pool     => nodeSlab, 
                             objSize  => Node'Size,
                             capacity => capacity);
    end setup;

    ---------------------------------------------------------------------------
    -- teardown
    ---------------------------------------------------------------------------
    procedure teardown with
        SPARK_Mode => Off
    is
    begin
        SlabAllocator.teardown (nodeSlab);
    end teardown;

    ---------------------------------------------------------------------------
    -- create
    ---------------------------------------------------------------------------
    procedure create (myList : in out List; capacity : in Natural) with
        SPARK_Mode => On
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
    procedure delete (myList : in out List) with
        SPARK_Mode => On
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
    procedure insertFront (myList : in out List; element : in T) with
        SPARK_Mode => Off
    is
        prevHead : constant NodePtr := myList.head;
        newNode  : constant NodePtr := new Node'(element => element,
                                                 next    => null,
                                                 prev    => null);
    begin
        if myList.length = myList.capacity then
            raise LinkedListException with "Exceeded list capacity";
        end if;

        if myList.length = 0 then
            newNode.prev    := newNode;
            newNode.next    := newNode;
            myList.head     := newNode;
            myList.tail     := newNode;
        else
            newNode.prev    := myList.head;
            newNode.next    := prevHead;
            prevHead.prev   := newNode;
            myList.head     := newNode;
        end if;

        myList.length := myList.length + 1;
    end insertFront;

    ---------------------------------------------------------------------------
    -- insertBack
    ---------------------------------------------------------------------------
    procedure insertBack (myList : in out List; element : in T) with
        SPARK_Mode => Off
    is
        prevTail : constant NodePtr := myList.tail;
        newNode  : constant NodePtr := new Node'(element => element,
                                                 next    => null,
                                                 prev    => null);
    begin
        if myList.length = myList.capacity then
            raise LinkedListException with "Exceeded list capacity";
        end if;

        if myList.length = 0 then
            newNode.prev    := newNode;
            newNode.next    := newNode;
            myList.head     := newNode;
            myList.tail     := newNode;
        else
            newNode.prev    := myList.tail;
            newNode.next    := myList.head;
            prevTail.next   := newNode;
            myList.tail     := newNode;
        end if;

        myList.length := myList.length + 1;
    end insertBack;

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (myList : in out List) with
        SPARK_Mode => Off
    is
        oldHead : NodePtr;
        newHead : NodePtr;
    begin
        if myList.length = 0 then
            raise ListEmptyException with "Cannot popFront on empty list";
        end if;

        oldHead         := myList.head;
        newHead         := myList.head.next;
        newHead.prev    := myList.head;
        myList.head     := newHead;

        free (oldHead);
        myList.length := myList.length - 1;
    end popFront;

    ---------------------------------------------------------------------------
    -- front
    ---------------------------------------------------------------------------
    function front (myList : in List) return T with
        SPARK_Mode => Off
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
    procedure popBack (myList : in out List) with
        SPARK_Mode => Off
    is
        oldTail : NodePtr;
        newTail : NodePtr;
    begin
        if myList.length = 0 then
            raise ListEmptyException with "Cannot popBack of empty list";
        end if;

        oldTail         := myList.tail;
        newTail         := myList.tail.prev;
        newTail.next    := myList.head;
        myList.tail     := newTail;

        free(oldTail);
        myList.length := myList.length - 1;
    end popBack;

    ---------------------------------------------------------------------------
    -- back
    ---------------------------------------------------------------------------
    function back (myList : in List) return T with
        SPARK_Mode => Off
    is
    begin
        if myList.length = 0 then
            raise ListEmptyException with "Cannot get back of empty list";
        end if;

        return myList.tail.element;
    end back;

    -- procedure remove(myList : in out List; value : in T) with
    --     SPARK_Mode => Off
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
    procedure clear (myList : in out List) with
        SPARK_Mode => On
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
    procedure print (myList : in List) with
        SPARK_Mode => On
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
