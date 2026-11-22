pragma Ada_2022;
package body Bridge_Trace is
 procedure Note(E : Event) is begin Used:=Used+1; Log(Used):=E; end Note;
end Bridge_Trace;
