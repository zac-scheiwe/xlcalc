Attribute VB_Name = "VbaRtDateB"
Option Explicit

Private Function Dump(v As Variant) As String
    Dim i As Long, s As String
    s = "A" & LBound(v) & ":" & UBound(v) & ":"
    For i = LBound(v) To UBound(v)
        If i > LBound(v) Then s = s & "|"
        s = s & v(i)
    Next i
    Dump = s
End Function

Private Function RtDateB0(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: RtDateB0 = DateSerial(a0, a1, a2)
    Case 1: RtDateB0 = TimeSerial(a0, a1, a2)
    Case 2: RtDateB0 = CDate(a0)
    Case 3: RtDateB0 = DateValue(a0)
    Case 4: RtDateB0 = TimeValue(a0)
    Case 5: RtDateB0 = IsDate(a0)
    Case 6: RtDateB0 = Year(a0)
    Case 7: RtDateB0 = Month(a0)
    Case 8: RtDateB0 = Day(a0)
    Case 9: RtDateB0 = Hour(a0)
    Case 10: RtDateB0 = Minute(a0)
    Case 11: RtDateB0 = Second(a0)
    Case 12: RtDateB0 = Weekday(a0)
    Case 13: RtDateB0 = Weekday(a0, a1)
    Case 14: RtDateB0 = DateAdd(a0, a1, a2)
    Case 15: RtDateB0 = DateDiff(a0, a1, a2)
    Case 16: RtDateB0 = DateDiff(a0, a1, a2, a3)
    Case 17: RtDateB0 = DateDiff(a0, a1, a2, a3, a4)
    Case 18: RtDateB0 = DateDiff(a0, a1, a2, , a4)
    End Select
    Exit Function
EH:
    RtDateB0 = "#ERR " & Err.Number
End Function

Public Function RtDateB(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    Select Case k \ 40
    Case 0: RtDateB = RtDateB0(k, a0, a1, a2, a3, a4, a5)
    End Select
End Function
