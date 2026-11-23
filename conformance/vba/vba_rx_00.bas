Attribute VB_Name = "VbaRx_00"
Option Explicit
Public Function VbRx0(ByVal k As Long) As Variant
    On Error GoTo EH
    Dim s As String
    R10Log = "": R10Mode = 0
    Select Case k
    Case 0
        Sheet1.Range("A1").Value = 1
        s = "warm"
    Case 12
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        s = R10Log & "|" & Application.EnableEvents
        R10Log = ""
        Application.EnableEvents = True
        s = s & "|" & R10Log
    Case 1
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        R10Log = ""
        Application.EnableEvents = False
        Application.EnableEvents = True
        s = R10Log
    Case 2
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        R10Log = ""
        Sheet1.Range("A1").Value = 5
        s = R10Log & "|" & Application.EnableEvents
    Case 3
        Application.EnableEvents = False
        Sheet1.Range("A1").Value = 5
        Application.EnableEvents = True
        s = R10Log
    Case 4
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        R10Log = ""
        Application.EnableEvents = False
        Sheet1.Range("B7").Value = 5
        Application.EnableEvents = True
        s = R10Log
    Case 5
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        R10Log = ""
        Application.EnableEvents = False
        Sheet1.Range("B7").Value = 5
        s = R10Log
        Application.EnableEvents = True
        s = s & "|" & R10Log
        Sheet1.Range("C9").Value = 1
        s = s & "|" & R10Log
    Case 6
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        R10Log = ""
        Sheet1.Range("A2").Formula = "=A1*2"
        s = R10Log
        Application.EnableEvents = True
        s = s & "|" & R10Log
    Case 7
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        s = R10Log & "|" & Application.EnableEvents
        Sheet1.Range("B1").Value = 3
        s = s & "|" & R10Log
    Case 8
        Sheet1.Range("A1").Value = 2
        s = R10Log
        Sheet1.Range("B1").Value = 3
        s = s & "|" & R10Log
    Case 9
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        s = R10Log
        Application.EnableEvents = True
        Sheet1.Range("B1").Value = 3
        s = s & "|" & R10Log
    Case 10
        R10Mode = 6
        Sheet1.Range("A1").Value = 2
        DoEvents
        s = R10Log
        R10Mode = 0
        Application.EnableEvents = True
        Sheet1.Range("B1").Value = 3
        s = s & "|" & R10Log
    Case 11
        R10Mode = 4
        Sheet1.Range("A1").Value = 2
        s = R10Log
        Sheet1.Range("B1").Value = 3
        s = s & "|" & R10Log
    End Select
    R10Reset
    Sheet1.Cells.Clear: Sheet2.Cells.Clear
    VbRx0 = s
    Exit Function
EH:
    VbRx0 = "#ERR " & Err.Number & " " & Err.Description
    R10Reset
End Function
