-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Linked List implementation
--
-- @description
-- Each time this package is instantiated, setup must be called with the 
-- _package-wide_ capacity. Underlying physical storage will be allocated in
-- setup. Individual lists created using this package will share that memory,
-- but per-list capacities can be set as well in the call to create.
-- 
-- @TODO would like an iterator here.
--
-- @TODO probably want some way in here to just get access to the underlying
-- objects vs potentially returning a copy of a large object.
-------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

with SlabAllocator;

generic
    type T is private;
    with procedure printElem(element : in T);

package LinkedLists with
    SPARK_Mode => On
is

    LinkedListException : exception;

    nodeSlab : SlabAllocator.Slab;

    type Node;
    type NodePtr is access Node;

    for NodePtr'Simple_Storage_Pool use nodeSlab;

    type Node is
    record
        next    : NodePtr;
        prev    : NodePtr;
        element : T;
    end record;

    -- List is the doubly-linked list head structure
    -- @field nodeSlab - slab allocator used for new nodes
    -- @field head - first node in this list
    -- @field tail - last node in this list
    -- @field length - number of elements in the list
    type List is
    record
        -- SlabAllocator used to allocate new nodes for the list
        --nodeSlab    : SlabAllocator.Slab;
        head        : NodePtr;
        tail        : NodePtr;
        capacity    : Natural := 0;
        length      : Natural := 0;
    end record;

    -- Currently the Slab Allocator only supports a single physical memory
    -- block with a specified order. If we try and create a list with a
    -- capacity greater than the biggest physical memory block, this exception
    -- will be thrown. In the future each List can probably keep an array of
    -- SlabAllocators for lists that need to span multiple slabs, but we'll
    -- need to keep track of which slab was used to allocate a particular node.
    ListTooBigException : exception;

    -- Trying to access or remove list elements on an empty list will raise
    -- this.
    ListEmptyException : exception;

    ---------------------------------------------------------------------------
    -- setup
    -- Initialize the storage for a particular LinkedList package instantiation.
    --
    -- @field capacity - initial number of objects this _package_ can allocate
    --  across all the Lists it is used to create.
    ---------------------------------------------------------------------------
    procedure setup (capacity : in Natural) with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- teardown
    -- Free the underlying slab used to allocate list nodes for this
    -- package. This doesn't bother to free individual list nodes back to the
    -- slab, since the slab's physical memory will be freed directly back to
    -- whatever physical allocator it came from.
    ---------------------------------------------------------------------------
    procedure teardown with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- create
    -- Create a new List object with given capacity.
    -- @CAUTION ensure that the capacity given is less than the capacity given
    --  to the setup function.
    ---------------------------------------------------------------------------
    procedure create (myList : in out List; capacity : in Natural) with
        SPARK_Mode => On;
    

    ---------------------------------------------------------------------------
    -- delete
    -- Free the memory used by any remaining nodes in the list and reset the
    -- list. create must be called on the list if it is to be used again.
    ---------------------------------------------------------------------------
    procedure delete (myList : in out List) with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- insertFront
    -- Insert an element at the front of this linked list
    ---------------------------------------------------------------------------
    procedure insertFront (myList : in out List; element : in T) with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- insertBack
    -- Insert an element at the back of this linked list.
    ---------------------------------------------------------------------------
    procedure insertBack (myList : in out List; element : in T) with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- popFront
    -- remove the first element in this list
    ---------------------------------------------------------------------------
    procedure popFront (myList : in out List) with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- front
    -- return the first element in this list
    ---------------------------------------------------------------------------
    function front (myList : in List) return T with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- popBack
    -- remove the last element in this list
    ---------------------------------------------------------------------------
    procedure popBack (myList : in out List) with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- back
    -- return the last element in this list
    ---------------------------------------------------------------------------
    function back (myList : in List) return T with
        SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- remove
    -- Remove an element from this list using linear search
    ---------------------------------------------------------------------------
    -- procedure remove(myList : in out List; value : in T) with
    --     SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- clear
    -- Remove all elements from this list
    ---------------------------------------------------------------------------
    procedure clear (myList : in out List) with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- isEmpty
    -- Return True if this list contains no elements, False otherwise.
    ---------------------------------------------------------------------------
    function isEmpty (myList : in List) return Boolean is (myList.length = 0)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- print
    -- Print all elements in this list by calling the generic parameter
    -- printElem on each.
    ---------------------------------------------------------------------------
    procedure print (myList : in List) with SPARK_Mode => On;

private
 
    procedure free is new Ada.Unchecked_Deallocation(object => Node,
                                                     name   => NodePtr);
end LinkedLists;