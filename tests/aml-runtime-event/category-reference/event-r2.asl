DefinitionBlock ("", "DSDT", 2, "CUBIT", "EVSLOTS", 1) {
 Event (EV00)
 Method (MAKE, 0, Serialized) { Event (TEMP) Return (TEMP) }
 Method (GET0, 0, Serialized) { Return (EV00) }
 Method (TMO0, 0, Serialized) { CopyObject (MAKE (), EV00) Return (Zero) }
 Method (DRET, 0, Serialized) { Reset (EV00) Signal (EV00) Return (Wait (GET0 (), TMO0 ())) }
 Method (LOCL, 0, Serialized) { CopyObject (MAKE (), Local0) Signal (Local0) Return (Wait (Local0, CopyObject (Zero, Local0))) }
 Method (PASS, 1, Serialized) { Signal (Arg0) Return (Wait (Arg0, CopyObject (Zero, Arg0))) }
 Method (ARGT, 0, Serialized) { Return (PASS (MAKE ())) }
}
