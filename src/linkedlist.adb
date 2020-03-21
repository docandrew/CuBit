-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Linked List implementation
-------------------------------------------------------------------------------
with Textmode; use Textmode;

package body LinkedList is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup(myList : in DList) with
        SPARK_Mode => Off
    is
    begin
        SlabAllocator.setup(myList.nodeSlab, 
    end setup;

    ---------------------------------------------------------------------------
    -- insertFront
    ---------------------------------------------------------------------------
    procedure insertFront(myList : in out List; element : in T) with
        SPARK_Mode => Off
    is
    begin
        myList.head := new Node'(element, myList.head);

        myList.length := myList.length + 1;
    end insertFront;

    ---------------------------------------------------------------------------
    -- insertBack
    ---------------------------------------------------------------------------
    procedure insertBack(myList : in out List; element : in T) with
        SPARK_Mode => Off
    is
        lastNode : NodePtr;
    begin
        -- empty list
        if myList.head = null then
            myList.head := new Node'(element, myList.head);
            myList.tail := myList.head;
        else
            lastNode := myList.tail;
            lastNode.next := new Node'(element, null);
            myList.tail := lastNode.next;
        end if;
        myList.length := myList.length + 1;
    end insertBack;


    procedure remove(myList : in out List; value : in T) with
        SPARK_Mode => Off
    is
        curNode    : NodePtr := myList.head;
        prevNode   : NodePtr := curNode;
    begin
        if curNode /= null and then curNode.element = value then
            -- is this the first element?
            myList.head := curNode.next;
            free(curNode);
            myList.length := myList.length - 1;

            -- update tail if there was just one element
            if myList.head = null then
                myList.tail := null;
            end if;
        else
            iterate : while curNode /= null
            loop    
                if curNode.element = value then
                    prevNode.next := curNode.next;
                    free(curNode);
                    myList.length := myList.length - 1;

                    -- update tail if this was the last element
                    if prevNode.next = null then
                        myList.tail := prevNode;
                    end if;
                    return;
                end if;

                prevNode := curNode;
                curNode := curNode.next;
            end loop iterate;
        end if;
    end remove;


    procedure print(myList : in List) with
        SPARK_Mode => On
    is
        curNode : NodePtr := myList.head;
    begin
        if curNode = null then
            println("empty");
            return;
        end if;

        while curNode /= null loop
            printElem(curNode.element);
            print(" ->");
            curNode := curNode.next;
        end loop;

        println;
    end print;

end LinkedList;