Attribute VB_Name = "MacroRun2"
Option Explicit

Public Function Dup() As Variant
    On Error GoTo EH
    Dup = "MacroRun2.Dup"
    Exit Function
EH:
    Dup = "err " & Err.Number
End Function

Public Function Only2() As Variant
    On Error GoTo EH
    Only2 = "Only2"
    Exit Function
EH:
    Only2 = "err " & Err.Number
End Function
