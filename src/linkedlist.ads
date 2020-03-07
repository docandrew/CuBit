-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Linked List implementation
-------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

with SlabAllocator;

generic
    type T is private;
    with procedure printElem(element : in T);
package LinkedList with
    SPARK_Mode => On
is

    type Node;
    type NodePtr is access Node;

    type Node is
    record
        next    : NodePtr;
        prev    : NodePtr;
        element : T;
    end record;

    -- DList is the doubly-linked list head structure
    -- @field nodeSlab - slab allocator used for new nodes
    -- @field head - first node in this list
    -- @field tail - last node in this list
    type DList is
    record
        -- SlabAllocator used to allocate new nodes for the list
        nodeSlab : SlabAllocator.Slab;
        
        length  : Natural := 0;
    end record;

    ---------------------------------------------------------------------------
    -- insertFront
    -- Insert an element at the front of this linked list
    ---------------------------------------------------------------------------
    procedure insertFront(myList : in out List; element : in T);

    ---------------------------------------------------------------------------
    -- insertBack
    -- Insert an element at the back of this linked list.
    ---------------------------------------------------------------------------
    procedure insertBack(myList : in out List; element : in T);

    ---------------------------------------------------------------------------
    -- popFront
    -- remove the first element in this list
    ---------------------------------------------------------------------------
    function popFront(myList : in out List) return T;

    ---------------------------------------------------------------------------
    -- front
    -- return the first element in this list
    ---------------------------------------------------------------------------
    function front(myList : in List) return T;

    ---------------------------------------------------------------------------
    -- back
    -- return the last element in this list
    ---------------------------------------------------------------------------
    function back(myList : in List) return T;

    ---------------------------------------------------------------------------
    -- remove
    -- Remove an element from this list using linear search
    ---------------------------------------------------------------------------
    procedure remove(myList : in out List; value : in T);

    ---------------------------------------------------------------------------
    -- isEmpty
    -- Return True if this list contains no elements, False otherwise.
    ---------------------------------------------------------------------------
    function isEmpty(myList : in List) return Boolean;

    ---------------------------------------------------------------------------
    -- print
    -- Print all elements in this list by calling the generic parameter
    -- printElem on each.
    ---------------------------------------------------------------------------
    procedure print(myList : in List);

private
 
    procedure free is new Ada.Unchecked_Deallocation(object => Node,
                                                     name => NodePtr);
end LinkedList;